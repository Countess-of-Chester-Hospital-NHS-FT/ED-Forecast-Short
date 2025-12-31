library(tidyverse)
library(janitor)
library(finalfit)
library(zoo)
library(slider)
library(patchwork)
library(scales)
library(prophet)
library(odbc)
library(DBI)

theme_set(theme_bw())

### globals
performance_period_start = ymd("2025-12-06")
performance_period_end = ymd("2025-12-20")
performance_period = performance_period_start%--%performance_period_end

### data import
db <- DBI::dbConnect(odbc::odbc(), "coch_p2")
ecds_data <-  DBI::dbGetQuery(db, "select * from InformationSandpitDB.[datascience].[EDForecastShort_training]")


#saveRDS(ecds_data, "ecds_data.RDS")
#ecds_data <- readRDS("ecds_data.RDS")

### data prep
data <- ecds_data |>
  clean_names() |>
  select(encntr_id, local_patient_identifier, check_in_date_time, check_in_date,
         check_in_hour, arrival_mode,
         age) |>
  mutate(check_in_week = floor_date(check_in_date, unit = "week", week_start = 1),
         check_in_hour_dt = floor_date(check_in_date_time, unit = "hour"),
         check_in_month = floor_date(check_in_date, unit = "month"),
         iso_week_number = isoweek(check_in_date_time),
         iso_year = isoyear(check_in_date_time),
         month_number = month(check_in_date_time),
         month_name = month(check_in_date_time, label = TRUE, abbr = FALSE),
         weekday_number = wday(check_in_date_time, week_start = 1), # Sunday = 1, Monday = 2, etc.
         weekday_name = wday(check_in_date_time, label = TRUE, abbr = TRUE, week_start = 1),
         year = year(check_in_date_time),
         ambulance_arrival = if_else(arrival_mode == "Ambulance", 1, 0, missing = 0),
         walk_arrival = if_else(arrival_mode != "Ambulance", 1, 0, missing = 1)
  )

missing_data <- missing_glimpse(data)
#saveRDS(data, "data.RDS")
#data <- readRDS("data.RDS") #for offline

## Make the time series - otherwise hours with 0 will be excluded
min_dt <- min(data$check_in_hour_dt)
max_dt <- max(data$check_in_hour_dt)

## Actuals
data_actuals <- data |>
  group_by(check_in_hour_dt) |>
  summarise(
    sum_walk = sum(walk_arrival),
    sum_amb = sum(ambulance_arrival)
  ) |>
  arrange(check_in_hour_dt)

hourly_series <- tibble(check_in_hour_dt = seq(min_dt, max_dt, by = "hour"))

data_actuals2 <- hourly_series |>
  left_join(data_actuals, by = "check_in_hour_dt") |>
  mutate(
    # Replace NAs with 0 for both columns
    sum_walk = replace_na(sum_walk, 0),
    sum_amb  = replace_na(sum_amb, 0)
  )

############ Baseline prediction
data_baseline <- data_actuals2 |>
  group_by(
    day_of_week = wday(check_in_hour_dt), 
    hour_of_day = hour(check_in_hour_dt)
  ) |>
  arrange(check_in_hour_dt) |>
  mutate(
    baseline_walk = slide_dbl(
      .x = lag(sum_walk),       
      .f = ~mean(.x, na.rm = TRUE), 
      .before = 11,      # The window includes the current (lagged) value plus the 11 before it
      .complete = FALSE # Allow calculations even if there are fewer than 12 previous values
    ),
    baseline_walk = if_else(baseline_walk != "NaN", round(baseline_walk), NA),
    baseline_amb = slide_dbl(
      .x = lag(sum_amb),       
      .f = ~mean(.x, na.rm = TRUE), 
      .before = 11,      # The window includes the current (lagged) value plus the 11 before it
      .complete = FALSE # Allow calculations even if there are fewer than 12 previous values
    ),
    baseline_amb = if_else(baseline_amb != "NaN", round(baseline_amb), NA)
  ) |>
  ungroup() |>
  select(-hour_of_day, -day_of_week) |>
  filter(check_in_hour_dt %within% performance_period,
         check_in_hour_dt < performance_period_end)

################# Prophet prediction

## prophet globals
#globals
pandemic_start <- ymd("2020-01-01")
pandemic_end <- ymd("2023-01-01")
pandemic <- pandemic_start%--%pandemic_end

## prophet pre-processing
#holidays
db <- DBI::dbConnect(odbc::odbc(), "coch_p2")
holidays <-  DBI::dbGetQuery(db,
                             "select
                        	Date_Skey
                        	,UKHolidayName
                        from [CCDW].[dim].[Date]
                        where Date_Skey > '20170101'
                        and IsUKHoliday = 1") |>
  clean_names()

years <- 2017:2025
interim_dates <- as.Date(paste0(years, "-12-28"))

interim_holiday <- data.frame(
  holiday = "christmas_interim",
  ds = interim_dates,
  lower_window = 0,
  upper_window = 2
)

holidays <- holidays |>
  rename(ds = date_skey,
         holiday = uk_holiday_name) |>
  filter(!holiday %in% c("Boxing Day", "Easter Sunday", "Easter Monday")) |>
  mutate(ds = ymd(ds),
         holiday = if_else(holiday == "Good Friday", "Easter", holiday),
         holiday = if_else(!holiday %in% c("Christmas Day", "New Years Day", "Easter"), "Bank Holiday", holiday)) |>
  mutate(lower_window = case_when(holiday == "Christmas Day" ~ -2,
                                  holiday == "New Years Day" ~ -1,
                                  holiday == "Bank Holiday" ~ 0,
                                  holiday == "Easter" ~ -1),
         upper_window = case_when(holiday == "Christmas Day" ~ 2,
                                  holiday == "New Years Day" ~ 1,
                                  holiday == "Bank Holiday" ~ 1,
                                  holiday == "Easter" ~ 4)) |>
  bind_rows(interim_holiday)

# remove pandemic period
data_no_pandemic <- data_actuals2 |>
  mutate(
    sum_walk = if_else(check_in_hour_dt %within% pandemic, NA, sum_walk),
    sum_amb = if_else(check_in_hour_dt %within% pandemic, NA, sum_amb)
    )

## prophet loop

date_seq <- seq(performance_period_start - days(2), today() - days(7), by = "day")
results_df <- tibble()

m <- prophet(holidays = holidays, 
             daily.seasonality = FALSE, 
             weekly.seasonality = TRUE) # yearly is auto true if > 2 years of data

# Add custom hourly seasonality for each specific day
m <- add_seasonality(m, name='hourly_mon', period=1, fourier.order=4, condition.name='is_mon')
m <- add_seasonality(m, name='hourly_tue', period=1, fourier.order=4, condition.name='is_tue')
m <- add_seasonality(m, name='hourly_wed', period=1, fourier.order=4, condition.name='is_wed')
m <- add_seasonality(m, name='hourly_thu', period=1, fourier.order=4, condition.name='is_thu')
m <- add_seasonality(m, name='hourly_fri', period=1, fourier.order=4, condition.name='is_fri')
m <- add_seasonality(m, name='hourly_sat', period=1, fourier.order=4, condition.name='is_sat')
m <- add_seasonality(m, name='hourly_sun', period=1, fourier.order=4, condition.name='is_sun')

add_day_indicators <- function(df) {
  df %>%
    mutate(
      is_mon = as.numeric(wday(ds) == 2),
      is_tue = as.numeric(wday(ds) == 3),
      is_wed = as.numeric(wday(ds) == 4),
      is_thu = as.numeric(wday(ds) == 5),
      is_fri = as.numeric(wday(ds) == 6),
      is_sat = as.numeric(wday(ds) == 7),
      is_sun = as.numeric(wday(ds) == 1)
    )
}

for (i in seq_along(date_seq)) {
  current_date <- date_seq[i]
  print(current_date)
  
  # split for training
  data_train <- data_no_pandemic |>
    filter(check_in_hour_dt < current_date)
  
  data_test <- data_no_pandemic |>
    filter(check_in_hour_dt >= current_date)
  
  print("Predicting walk-ins...")
  
  ### walk
  df_walk <- data.frame(
    ds = data_train$check_in_hour_dt,
    y = data_train$sum_walk
  )
  df_walk <- add_day_indicators(df_walk)
  
  m_walk <- fit.prophet(m, df_walk)
  
  future_walk <- make_future_dataframe(m_walk, periods = 168, freq = 'hour', include_history = FALSE)
  future_walk <- add_day_indicators(future_walk)
  forecast_walk <- predict(m_walk, future_walk)
  test_actual <- head(data_test, 168)
  
  print("Predicting ambulances...")
  
  ### ambulance
  df_amb <- data.frame(
    ds = data_train$check_in_hour_dt,
    y = data_train$sum_amb
  )
  df_amb <- add_day_indicators(df_amb)
  
  m_amb <- fit.prophet(m, df_amb)
  
  future_amb <- make_future_dataframe(m_amb, periods = 168, freq = 'hour', include_history = FALSE)
  future_amb <- add_day_indicators(future_amb)
  forecast_amb <- predict(m_amb, future_amb)
  ####
  
  comparison <- data.frame(
    ds = test_actual$check_in_hour_dt,
    actual_walk = test_actual$sum_walk,
    predicted_walk = round(forecast_walk$yhat),
    actual_amb = test_actual$sum_amb,
    predicted_amb = round(forecast_amb$yhat),
    forecast_date = current_date
  ) |>
    mutate(days_ahead = dense_rank(as.Date(ds)))
  
  results_df <- results_df |>
    bind_rows(comparison)
  
}

results_df <- results_df |>
  filter(ds %within% performance_period,
         ds < performance_period_end)

######## Comparing performance daily level

## mae and mape at a daily level for baseline
baseline_daily <- data_baseline |>
  mutate(
    total_actual = sum_walk + sum_amb,
    total_baseline = baseline_walk + baseline_amb
  ) |>
  group_by(as_date(check_in_hour_dt)) |>
  summarise(
    daily_actual = sum(total_actual),
    daily_baseline = sum(total_baseline)
  ) |>
  mutate(
    diff = daily_baseline - daily_actual,
    abs_diff = abs(diff),
    p_error = (abs_diff/daily_actual) * 100
  )

baseline_performance <- baseline_daily |>
  summarise(
    mae = mean(abs_diff),
    mape = mean(p_error)
  )

## mae and mape at a daily level for prophet
prophet_daily <- results_df |>
  filter(days_ahead == 2) |>
  mutate(
    total_actual = actual_walk + actual_amb,
    total_prophet = predicted_walk + predicted_amb
  ) |>
  group_by(as_date(ds)) |>
  summarise(
    daily_actual = sum(total_actual),
    daily_prophet = sum(total_prophet)
  ) |>
  mutate(
    diff = daily_prophet - daily_actual,
    abs_diff = abs(diff),
    p_error = (abs_diff/daily_actual) * 100
  )

prophet_performance <- prophet_daily |>
  summarise(
    mae = mean(abs_diff),
    mape = mean(p_error)
  )

## Diff histogram daily level
daily_hist_df0 <- baseline_daily |>
  rename(date = `as_date(check_in_hour_dt)`) |>
  select(date, diff) |>
  mutate(model = "baseline")

daily_hist_df <- prophet_daily |>
  rename(date = `as_date(ds)`) |>
  select(date, diff) |>
  mutate(model = "prophet") |>
  bind_rows(daily_hist_df0)

daily_hist_df |>
  ggplot(aes(x = diff)) +
  geom_histogram(color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~model, ncol = 1) +
  labs(x = "Difference between Prediction and Actual",
       y = "Count",
       title = "Error Distributions")

## Linechart
daily_line_df0 <- baseline_daily |>
  rename(date = `as_date(check_in_hour_dt)`) |>
  select(date, daily_baseline) |>
  rename(value = daily_baseline) |>
  mutate(model = "baseline")

daily_line_df1 <- baseline_daily |>
  rename(date = `as_date(check_in_hour_dt)`) |>
  select(date, daily_actual) |>
  rename(value = daily_actual) |>
  mutate(model = "actual")

daily_line_df <- prophet_daily |>
  rename(date = `as_date(ds)`) |>
  select(date, daily_prophet) |>
  rename(value = daily_prophet) |>
  mutate(model = "prophet") |>
  bind_rows(daily_line_df0, daily_line_df1)

library(plotly)

p <- daily_line_df |>
  ggplot(aes(x = date, y = value, color = model)) +
  geom_line(aes(linewidth = model == "actual")) +
  geom_point(aes(size = model == "actual")) +
  scale_linewidth_manual(values = c("TRUE" = 1.0, "FALSE" = 0.5), guide = "none") +
  scale_size_manual(
    values = c("TRUE" = 1.8, "FALSE" = 1.2), 
    guide = "none"
  ) +
  labs(x = NULL,
       y = "Value",
       title = "Predictions vs actuals")

ggplotly(p) |> 
  layout(
    # This creates the "box" look of theme_bw()
    xaxis = list(mirror = TRUE, linecolor = "black", showline = TRUE),
    yaxis = list(mirror = TRUE, linecolor = "black", showline = TRUE)
  )

## wape for baseline
baseline_wape <- data_baseline |>
  mutate(
    total_actual = sum_walk + sum_amb,
    total_baseline = baseline_walk + baseline_amb
  ) |>
  select(check_in_hour_dt, total_actual, total_baseline) |>
  mutate(
    diff = total_baseline - total_actual,
    abs_diff = abs(diff)
  ) 

baseline_wape2 <- baseline_wape |>
  group_by(as_date(check_in_hour_dt)) |>
  summarise(
    sum_diff = sum(abs_diff),
    sum_actual = sum(total_actual)
  ) |>
  ungroup() |>
  mutate(wape = (sum_diff / sum_actual) * 100) |>
  summarise(
    overall_wape = mean(wape)
  )

## wape for prophet
prophet_wape <- results_df |>
  filter(days_ahead == 2) |>
  mutate(
    total_actual = actual_walk + actual_amb,
    total_prophet = predicted_walk + predicted_amb
  ) |>
  select(ds, total_actual, total_prophet) |>
  mutate(
    diff = total_prophet - total_actual,
    abs_diff = abs(diff)
  ) 

prophet_wape2 <- prophet_wape |>
  group_by(as_date(ds)) |>
  summarise(
    sum_diff = sum(abs_diff),
    sum_actual = sum(total_actual)
  ) |>
  ungroup() |>
  mutate(wape = (sum_diff / sum_actual) * 100) |>
  summarise(
    overall_wape = mean(wape)
  )

## Diff histogram hourly level
hourly_hist_df0 <- baseline_wape |>
  rename(datetime = check_in_hour_dt) |>
  select(datetime, diff) |>
  mutate(model = "baseline")

hourly_hist_df <- prophet_wape |>
  rename(datetime = ds) |>
  select(datetime, diff) |>
  mutate(model = "prophet") |>
  bind_rows(hourly_hist_df0)

hourly_hist_df |>
  ggplot(aes(x = diff)) +
  geom_histogram(color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~model, ncol = 1) +
  labs(x = "Difference between Prediction and Actual",
       y = "Count",
       title = "Error Distributions")
  
  