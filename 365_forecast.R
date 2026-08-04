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
options(lubridate.week.start = 1) # week starts on monday

### globals
#step_change = "2024-07-01" # T3 step change

### data import
db <- DBI::dbConnect(odbc::odbc(), "coch_p2")
data <-  DBI::dbGetQuery(db, "select * from InformationSandpitDB.[datascience].[ED_Prophet12mo_training]") |>
  clean_names()

### create dataframe of actuals making sure all dates in the series are accounted for
data_actuals <- data |>
  group_by(check_in_date) |>
  count() |>
  arrange(check_in_date)

min_dt <- min(data$check_in_date)
max_dt <- max(data$check_in_date)
date_series <- tibble(check_in_date = seq(min_dt, max_dt, by = "day"))

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


############################################## Wrangling holidays and pandemic
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

daily_forecast_tidy <- forecast |>
  select(
    ds,
    yhat,
    yhat_lower,
    yhat_upper
  ) |>
  rename(
    date = ds,
    attendances = yhat,
    daily_lower_interval = yhat_lower,
    daily_upper_interval = yhat_upper
  ) |>
  mutate(
    date = date(date),
    month = floor_date(date, unit = "month"),
    week = floor_date(date, unit = "week"),
    weekday = wday(date, label = TRUE),
    forecast = TRUE,
    days_ahead = as.integer(date - today()) + 1,
    dept_type = "Total"
  ) |>
  select(
    forecast,
    date,
    week,
    weekday,
    month,
    days_ahead,
    dept_type,
    attendances,
    daily_lower_interval,
    daily_upper_interval,
  ) 


################# run 1000 simulations
samples <- predictive_samples(
  m_forecast,
  df_futuredates
)

daily_simulations0 <- samples$yhat |>
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

## add actual values from current week / current month to daily simulations
current_week_start <- floor_date(today(), unit = "week")
current_month_start <- floor_date(today(), unit = "month")

earliest_date <- min(current_week_start, current_month_start)

daily_simulations <- data_actuals2 |>
  filter(check_in_date >= earliest_date) |>
  uncount(weights = 1000, .id = "simulation") |>
  rename(
    ds = check_in_date,
    attendances = n
  ) |>
  bind_rows(daily_simulations0)

################## calculate intervals for monthly intervals

monthly_simulations <- daily_simulations |>
  mutate(month = floor_date(ds, "month")) |>
  group_by(simulation, month) |>
  summarise(
    average_daily = mean(attendances),
    .groups = "drop"
  ) |>
  arrange(month)

monthly_intervals <- monthly_simulations |>
  group_by(month) |>
  summarise(
    monthly_lower_interval = quantile(
      average_daily,
      probs = 0.10,
      na.rm = TRUE
    ),
    monthly_upper_interval = quantile(
      average_daily,
      probs = 0.90,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

############## calculate intervals for weekly intervals

weekly_simulations <- daily_simulations |>
  mutate(week = floor_date(ds, "week")) |>
  group_by(simulation, week) |>
  summarise(
    average_daily = mean(attendances),
    .groups = "drop"
  ) |>
  arrange(week)

weekly_intervals <- weekly_simulations |>
  group_by(week) |>
  summarise(
    weekly_lower_interval = quantile(
      average_daily,
      probs = 0.10,
      na.rm = TRUE
    ),
    weekly_upper_interval = quantile(
      average_daily,
      probs = 0.90,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

############## join back to daily figures

forecast_for_export <- daily_forecast_tidy |>
  left_join(weekly_intervals, by = "week") |>
  left_join(monthly_intervals, by = "month") |>
  select(-weekday, -week, -month)


############# export to staging table

# Connect to the database
con <- dbConnect(odbc::odbc(), 
                 DSN = "coch_p2",
                 Database = "InformationSandpitDB")

# 1. Write predictions to a staging table
dbWriteTable(con, 
             name = Id(schema = "datascience", table = "ED_Prophet12mo_stage"),
             value = forecast_for_export,
             overwrite = TRUE)





