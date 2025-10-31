library(tidyverse)
library(janitor)
library(finalfit)
library(zoo)
library(slider)

theme_set(theme_bw())

### data import
db <- DBI::dbConnect(odbc::odbc(), "coch_p2")
ecds_data <-  DBI::dbGetQuery(db, "select * from InformationSandpitDB.Reports.pbi_ED")

### data prep
data <- ecds_data |>
  clean_names() |>
  select(encntr_id, local_patient_identifier, check_in_date_time, check_in_date,
         check_in_hour, acuity, arrival_mode, attendance_source, discharge_destination,
         age) |>
  mutate(check_in_week = floor_date(check_in_date, unit = "week", week_start = 1),
         check_in_month = floor_date(check_in_date, unit = "month"),
         iso_week_number = isoweek(check_in_date_time),
         iso_year = isoyear(check_in_date_time),
         month_number = month(check_in_date_time),
         month_name = month(check_in_date_time, label = TRUE, abbr = FALSE),
         weekday_number = wday(check_in_date_time, week_start = 1), # Sunday = 1, Monday = 2, etc.
         weekday_name = wday(check_in_date_time, label = TRUE, abbr = TRUE, week_start = 1),
         year = year(check_in_date_time)
  ) |>
  filter(check_in_date >= ymd("2022-11-01"))

#saveRDS(data, "data.RDS")
data <- readRDS("data.RDS") #for offline

missing_data <- missing_glimpse(data)

### Plot of daily attends
plot_df <- data |>
  count(check_in_date, weekday_name)|>
  arrange(check_in_date) |>
  # Group by weekday to calculate the forecast for each day independently
  group_by(weekday_name) |>
  # Create the forecast column
  mutate(
    baseline_forecast = slide_dbl(
      .x = lag(n),       
      .f = ~mean(.x, na.rm = TRUE), 
      .before = 11,      # The window includes the current (lagged) value plus the 11 before it
      .complete = FALSE # Allow calculations even if there are fewer than 12 previous values
    ),
    baseline_forecast = round(baseline_forecast),
    diff = baseline_forecast - n
  )|>
  rename(actual_values = n) |>
  ungroup() |>
  filter(check_in_date >= ymd("2025-09-01"))

plot_df2 <- plot_df |>
  pivot_longer(
    cols = c(actual_values, baseline_forecast),
    names_to = "type",
    values_to = "value"
  )


plot_df2 |>
  ggplot(aes(x = check_in_date, y = value, color = type)) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = c(
    "actual_values" = "black",
    "baseline_forecast" = "deeppink3"
  )) +
  labs(color = NULL,
       x = NULL,
       y = "Attendances",
       title = "ED Attendances vs Baseline Values")

plot_df |>
  mutate(sign = ifelse(diff >= 0, "Positive", "Negative")) |>
  ggplot(aes(x = check_in_date, y = diff, fill = sign)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Positive" = "darkcyan", # Earthy, desaturated green
      "Negative" = "plum4"  # Muted plum/violet
    )) +
  labs(fill = NULL,
       x = NULL,
       y = "Prediction Error",
       title = "Baseline Forecast Prediction Error") +
  theme(legend.position = "none")


## All error distributions

    