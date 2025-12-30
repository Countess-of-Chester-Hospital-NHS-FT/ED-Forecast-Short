library(tidyverse)
library(janitor)
library(prophet)

theme_set(theme_bw())

data <- readRDS("plot_df.RDS")

#globals
pandemic_start <- ymd("2020-01-01")
pandemic_end <- ymd("2023-01-01")
pandemic <- pandemic_start%--%pandemic_end

date_seq <- seq(ymd("2024-12-30"), ymd("2025-12-29"), by = "day")

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
data_no_pandemic <- data |>
  mutate(actual_values = if_else(check_in_date %within% pandemic, NA, actual_values))

############## where the training loop goes ###################################

results_df <- tibble()

for (i in seq_along(date_seq)) {
  current_date <- date_seq[i]
  print(current_date)
  
  # split for training
  data_train <- data_no_pandemic |>
    filter(check_in_date < current_date)
  
  data_test <- data_no_pandemic |>
    filter(check_in_date >= current_date)
  
  df <- data.frame(
    ds = data_train$check_in_date,
    y = data_train$actual_values
  )
  
  #m <- prophet(df) # train the model
  m <- prophet(df, holidays = holidays)
  
  future <- make_future_dataframe(m, periods = 7)
  forecast <- predict(m, future)
  
  test_forecast <- tail(forecast, 7)
  test_actual <- head(data_test, 7)
  
  forecast_date = max(data_train$check_in_date)
  
  comparison <- data.frame(
    ds = test_actual$check_in_date,
    actual = test_actual$actual_values,
    predicted = round(test_forecast$yhat),
    forecast_date = forecast_date
  ) |>
    mutate(days_ahead = row_number())
  
  results_df <- results_df |>
    bind_rows(comparison)
}

performance_df <- tibble(
  days_ahead = integer(),
  mae = numeric(),
  mape = numeric()
)

for (i in 1:7) {
  day_df <- results_df |>
    filter(days_ahead == i)
  
  mae <- mean(abs(day_df$actual - day_df$predicted))
  mape <- mean(abs((day_df$actual - day_df$predicted) / day_df$actual)) * 100
  
  performance_df <- add_row(performance_df, days_ahead = i, mae = mae, mape = mape)
}

saveRDS(results_df, "results_df.RDS")




# Calculate accuracy
#mae <- mean(abs(comparison$actual - comparison$predicted))
#mape <- mean(abs((comparison$actual - comparison$predicted) / comparison$actual)) * 100

###############################################################################

# Plot forecast
#plot(m, forecast)

# Plot components (trend, weekly, yearly seasonality)
#prophet_plot_components(m, forecast)