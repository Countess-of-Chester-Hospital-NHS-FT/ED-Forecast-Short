library(tidyverse)
library(janitor)
library(prophet)

theme_set(theme_bw())

data <- readRDS("plot_df.RDS")

#globals
pandemic_start <- ymd("2020-01-01")
pandemic_end <- ymd("2023-01-01")
pandemic <- pandemic_start%--%pandemic_end

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

holidays <- holidays |>
  rename(ds = date_skey,
         holiday = uk_holiday_name) |>
  mutate(ds = ymd(ds),
         lower_window = -1,
         upper_window = 1)

# remove pandemic period
data_no_pandemic <- data |>
  mutate(actual_values = if_else(check_in_date %within% pandemic, NA, actual_values))

############## where the training loop goes ###################################

# split for training
data_train <- data_no_pandemic |>
  filter(check_in_date < ymd("2025-01-01"))

data_test <- data_no_pandemic |>
  filter(check_in_date >= ymd("2025-01-01"))

df <- data.frame(
  ds = data_train$check_in_date,
  y = data_train$actual_values
)

m <- prophet(df)
#m <- prophet(df, holidays = holidays)

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
  mutate(row_number = row_number())


# Calculate accuracy
mae <- mean(abs(comparison$actual - comparison$predicted))
mape <- mean(abs((comparison$actual - comparison$predicted) / comparison$actual)) * 100

###############################################################################

# Plot forecast
plot(m, forecast)

# Plot components (trend, weekly, yearly seasonality)
prophet_plot_components(m, forecast)
