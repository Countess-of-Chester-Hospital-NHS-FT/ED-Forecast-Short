library(tidyverse)
library(janitor)
library(finalfit)
library(zoo)
library(slider)
library(prophet)
library(odbc)
library(DBI)
library(ggtext)

theme_set(theme_bw())

### data import
db <- DBI::dbConnect(odbc::odbc(), "coch_p2")
ecds_data <-  DBI::dbGetQuery(db, "select * from InformationSandpitDB.[datascience].[EDForecastShort_training]")

### data prep
data <- ecds_data |>
  clean_names() |>
  select(encntr_id, local_patient_identifier, check_in_date_time, check_in_date,
         check_in_hour, arrival_mode,
         age) |>
  mutate(
    check_in_hour_dt = floor_date(check_in_date_time, unit = "hour"),
    check_in_date = floor_date(check_in_date_time, unit = "day"),
    ambulance_arrival = if_else(arrival_mode == "Ambulance", 1, 0, missing = 0),
    walk_arrival = if_else(arrival_mode != "Ambulance", 1, 0, missing = 1)
  ) |>
  filter(
    check_in_date < floor_date(today(), unit = "month")
  )

## Make the actuals date series - otherwise hours with 0 will be excluded
min_dt <- min(data$check_in_date)
max_dt <- max(data$check_in_date)
date_series <- tibble(check_in_date = seq(min_dt, max_dt, by = "day"))

## Actuals
data_actuals <- data |>
  group_by(check_in_date) |>
  count() |>
  arrange(check_in_date)

data_actuals2 <- date_series |>
  left_join(data_actuals, by = "check_in_date") |>
  mutate(
    # Replace NAs with 0 for both columns
    n = replace_na(n, 0)
  )


## Wrangling holidays and pandemic
pandemic_start <- ymd("2020-01-01")
pandemic_end <- ymd("2023-01-01")
pandemic <- pandemic_start%--%pandemic_end

# create holidays dataframe ready as prophet argument
db <- DBI::dbConnect(odbc::odbc(), "coch_p2")
holidays <-  DBI::dbGetQuery(db,
                             "select
                        	Date_Skey
                        	,UKHolidayName
                        from [CCDW].[dim].[Date]
                        where Date_Skey > '20170101'
                        and IsUKHoliday = 1") |>
  clean_names()

DBI::dbDisconnect(db)

## Check holidays table
if (nrow(holidays) < 140) {
  cat(sprintf("Script aborted due to issues with holidays table: %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("Issue with holidays table")
}

years <- 2017:2025


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
                                  holiday == "Easter" ~ 4))


################ remove pandemic period from the actuals
data_no_pandemic <- data_actuals2 |>
  mutate(
    n = if_else(check_in_date %within% pandemic, NA, n)
  )


############### setup prophet

m <- prophet(holidays = holidays, 
             daily.seasonality = FALSE, 
             weekly.seasonality = TRUE) # yearly is auto true if > 2 years of data


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

df_forecastinput <- data.frame(
  ds = data_no_pandemic$check_in_date,
  y = data_no_pandemic$n
)
df_forecastinput <- add_day_indicators(df_forecastinput)

m_forecast <- fit.prophet(m, df_forecastinput)

df_futuredates <- make_future_dataframe(m_forecast, periods = 365, freq = 'day', include_history = FALSE)
df_futuredates <- add_day_indicators(df_futuredates)
forecast <- predict(m_forecast, df_futuredates)


######### wrangle output

forecast_tidy <- forecast |>
  select(
    ds,
    yhat,
    yhat_lower,
    yhat_upper
  ) |>
  rename(
    date = ds,
    prediction = yhat,
    lower_interval = yhat_lower,
    upper_interval = yhat_upper
  ) |>
  mutate(
    #across(where(is.numeric), ~ round(.x, 0)),
    weekday = wday(date, label = T, abbr = F)
  ) |>
  left_join(holidays, by = c("date" = "ds")) |>
  select(
    date,
    weekday,
    prediction,
    lower_interval,
    upper_interval,
    holiday
  ) |>
  filter(
    #date %within% (ymd("260801")%--%ymd("260930")),
    date < floor_date(today(), unit = "month") %m+% months(12)
  ) |>
  mutate(
    date = date(date)
  )

forecast_monthly_average <- forecast_tidy |>
  mutate(
    month = floor_date(date, unit = "month")
  ) |>
  group_by(month) |>
  summarise(
    n = n(),
    sum_prediction = sum(prediction),
    sum_lower = sum(lower_interval),
    sum_upper = sum(upper_interval)
  ) |>
  mutate(
    average_daily_central = sum_prediction / n,
    average_daily_lower = sum_lower / n,
    average_daily_upper = sum_upper / n
  )

### simulations
samples <- predictive_samples(
  m_forecast,
  df_futuredates
)

daily_simulations <- samples$yhat |>
as.data.frame() |>
mutate(ds = df_futuredates$ds) |>
pivot_longer(
  cols = -ds,
  names_to = "simulation",
  values_to = "attendances"
) |>
mutate(
  simulation = as.integer(sub("V", "", simulation))
)

monthly_simulations <- daily_simulations |>
  mutate(month = floor_date(ds, "month")) |>
  group_by(simulation, month) |>
  summarise(
    average_daily = mean(attendances),
    .groups = "drop"
  )

monthly_forecast <- monthly_simulations |>
  group_by(month) |>
  summarise(
    average_daily_central = median(average_daily),
    average_daily_lower = quantile(
      average_daily,
      probs = 0.10,
      na.rm = TRUE
    ),
    average_daily_upper = quantile(
      average_daily,
      probs = 0.90,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


write_rds(forecast_monthly_average, "prophet_12monthly_average.RDS")
write_rds(monthly_forecast, "prophet_12monthly_average2.RDS")

stop("Temp stop")

######## export output
write_csv(forecast_tidy, "forecast_12month.csv")


######### plot output



forecast_tidy |>
  ggplot(aes(x = date, y = prediction)) +
  geom_vline(
    xintercept = ymd("260831"),
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = ymd("260831"),
    y = Inf,
    label = "Bank Holiday",
    angle = 90,
    vjust = 1.5,
    hjust = 1.1
  ) +
  geom_ribbon(
    aes(ymin = lower_interval, ymax = upper_interval),
    fill = "#3182bd",
    alpha = 0.25
  ) +
  geom_line() +
  geom_point() +
  scale_x_date(
    breaks = seq(
      from = ymd("260801"),
      to   = ymd("260930"),
      by   = "3 days"
    ),
    date_labels = "%d %b",
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Daily forecast of attendances August and September",
    subtitle = "<span style='color:#3182bd'>■</span> 80% Prediction Intervals",
    x = NULL,
    y = "Predicted Attendances"
  )+
  theme(plot.subtitle = element_markdown())
