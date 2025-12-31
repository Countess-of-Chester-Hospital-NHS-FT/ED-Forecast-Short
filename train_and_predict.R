library(dplyr)      
library(lubridate)  
library(tibble)     
library(tidyr)      
library(janitor)
library(finalfit)
library(zoo)
library(slider)
library(prophet)
library(odbc)
library(DBI)

cat(sprintf("Script started: %s\n", now()), 
    file = "script_log.txt", 
    append = TRUE)

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
         ambulance_arrival = if_else(arrival_mode == "Ambulance", 1, 0, missing = 0),
         walk_arrival = if_else(arrival_mode != "Ambulance", 1, 0, missing = 1)
  )

## Make the actuals time series - otherwise hours with 0 will be excluded
min_dt <- min(data$check_in_hour_dt)
max_dt <- max(data$check_in_hour_dt)
hourly_series <- tibble(check_in_hour_dt = seq(min_dt, max_dt, by = "hour"))

## Actuals
data_actuals <- data |>
  group_by(check_in_hour_dt) |>
  summarise(
    sum_walk = sum(walk_arrival),
    sum_amb = sum(ambulance_arrival)
  ) |>
  arrange(check_in_hour_dt)

data_actuals2 <- hourly_series |>
  left_join(data_actuals, by = "check_in_hour_dt") |>
  mutate(
    # Replace NAs with 0 for both columns
    sum_walk = replace_na(sum_walk, 0),
    sum_amb  = replace_na(sum_amb, 0)
  )

###### check data both ambulances and walkins are within plausible range for yesterday
check_input <- data |>
  group_by(check_in_date) |>
  summarise(
    n = n(),
    sum_walk = sum(walk_arrival),
    sum_amb = sum(ambulance_arrival)
  ) |>
  ungroup() |>
  filter(check_in_date == today() - 1)

if (check_input$n < 175 | check_input$sum_amb < 15) {
  cat(sprintf("Script aborted due to possible issue with ecds_attendances - yesterday attends < 175 or ambulances < 15: %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("Issue with ecds_attendances")
}
#########

## Wrangling holidays and pandemic
pandemic_start <- ymd("2020-01-01")
pandemic_end <- ymd("2023-01-01")
pandemic <- pandemic_start%--%pandemic_end

cat(sprintf("Retreiving holidays: %s\n", now()), 
    file = "script_log.txt", 
    append = TRUE)

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


################ remove pandemic period from the actuals
data_no_pandemic <- data_actuals2 |>
  mutate(
    sum_walk = if_else(check_in_hour_dt %within% pandemic, NA, sum_walk),
    sum_amb = if_else(check_in_hour_dt %within% pandemic, NA, sum_amb)
  )

########### setup prophet

cat(sprintf("Starting predictions: %s\n", now()), 
    file = "script_log.txt", 
    append = TRUE)

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

print("Predicting walk-ins...")

df_walk <- data.frame(
  ds = data_no_pandemic$check_in_hour_dt,
  y = data_no_pandemic$sum_walk
)
df_walk <- add_day_indicators(df_walk)

m_walk <- fit.prophet(m, df_walk)

future_walk <- make_future_dataframe(m_walk, periods = 168, freq = 'hour', include_history = FALSE)
future_walk <- add_day_indicators(future_walk)
forecast_walk <- predict(m_walk, future_walk)

print("Predicting ambulances...")

df_amb <- data.frame(
  ds = data_no_pandemic$check_in_hour_dt,
  y = data_no_pandemic$sum_amb
)
df_amb <- add_day_indicators(df_amb)

m_amb <- fit.prophet(m, df_amb)

future_amb <- make_future_dataframe(m_amb, periods = 168, freq = 'hour', include_history = FALSE)
future_amb <- add_day_indicators(future_amb)
forecast_amb <- predict(m_amb, future_amb) |> select(ds, yhat, yhat_lower, yhat_upper)

## processing for input
forecast_walk_1 <- forecast_walk |>
  select(ds, yhat, yhat_lower, yhat_upper) |>
  rename(
      yhat_walk = yhat,
      yhat_lower_walk = yhat_lower,
      yhat_upper_walk = yhat_upper
      )

forecast_combined <- forecast_amb |>
  select(ds, yhat, yhat_lower, yhat_upper) |>
  rename(
    yhat_amb = yhat,
    yhat_lower_amb = yhat_lower,
    yhat_upper_amb = yhat_upper
  ) |>
  left_join(forecast_walk_1, by = "ds") |>
  mutate(
    across(where(is.numeric), ~pmax(round(.x), 0)),
    yhat_total = yhat_walk + yhat_amb,
    days_ahead = dense_rank(as.Date(ds)),
    forecast_datetime = now()
  ) |>
  select(ds, yhat_total, yhat_walk, yhat_amb, days_ahead, forecast_datetime, everything())


## add baseline for comparison
print("Adding baseline predictions")

future_datetimes <- forecast_combined |>
  select(ds)

data_baseline <- data_actuals2 |>
  rename(ds = check_in_hour_dt) |>
  bind_rows(future_datetimes) |>
  group_by(
    day_of_week = wday(ds), 
    hour_of_day = hour(ds)
  ) |>
  arrange(ds) |>
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
    baseline_amb = if_else(baseline_amb != "NaN", round(baseline_amb), NA),
    baseline_total = baseline_walk + baseline_amb
  ) |>
  ungroup() |>
  select(-hour_of_day, -day_of_week, -sum_walk, -sum_amb)

tail <- tail(data_baseline, 168)

forecast_combined_all <- forecast_combined |>
  left_join(tail, by = "ds") |>
  select(ds, yhat_total, yhat_walk, yhat_amb, days_ahead, forecast_datetime,
         baseline_total, baseline_walk, baseline_amb, everything())

cat(sprintf("Starting data export: %s\n", now()), 
    file = "script_log.txt", 
    append = TRUE)

################## Data export
print("Export to database table")

#stop("Temp stop") # for development

# Connect to the database
con <- dbConnect(odbc::odbc(), 
                 DSN = "coch_p2",
                 Database = "InformationSandpitDB")

# 1. Write predictions to a staging table
dbWriteTable(con, 
             name = Id(schema = "datascience", table = "EDForecastShort_predictions_stage"),
             value = forecast_combined_all,
             overwrite = TRUE)


# 2. Run the same merge SQL to upsert into the main table
merge_sql <- "
MERGE datascience.EDForecastShort_predictions AS target
USING datascience.EDForecastShort_predictions_stage AS source
ON target.ds = source.ds AND target.days_ahead = source.days_ahead
WHEN MATCHED THEN
    UPDATE SET 
        ds = source.ds,
        yhat_total = source.yhat_total,
        yhat_walk = source.yhat_walk,
        yhat_amb = source.yhat_amb,
        days_ahead = source.days_ahead,
        forecast_datetime = source.forecast_datetime,
        baseline_total = source.baseline_total,
        baseline_walk = source.baseline_walk,
        baseline_amb = source.baseline_amb,
        yhat_lower_amb = source.yhat_lower_amb,
        yhat_upper_amb = source.yhat_upper_amb,
        yhat_lower_walk = source.yhat_lower_walk,
        yhat_upper_walk = source.yhat_upper_walk
WHEN NOT MATCHED THEN
    INSERT (
        ds, yhat_total, yhat_walk, yhat_amb, days_ahead, forecast_datetime,
        baseline_total, baseline_walk, baseline_amb, yhat_lower_amb, yhat_upper_amb, yhat_lower_walk,
        yhat_upper_walk
    )
    VALUES (
        source.ds, 
        source.yhat_total, 
        source.yhat_walk, 
        source. yhat_amb, 
        source.days_ahead, 
        source.forecast_datetime,
        source.baseline_total, 
        source.baseline_walk, 
        source.baseline_amb, 
        source.yhat_lower_amb, 
        source.yhat_upper_amb, 
        source.yhat_lower_walk,
        source.yhat_upper_walk
    );
"

# Execute the merge SQL
dbExecute(con, merge_sql)

# Close the connection when finished
dbDisconnect(con)
