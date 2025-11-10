library(tidyverse)
library(janitor)
library(finalfit)
library(zoo)
library(slider)
library(patchwork)
library(scales)

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

### df for plotting actuals, baseline and diffs
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
    diff = baseline_forecast - n,
    abs_diff = abs(diff),
    p_error = abs_diff/n
  )|>
  rename(actual_values = n) |>
  ungroup() |>
  filter(check_in_date >= ymd("2024-10-30"))

# df for plotting actual and baseline forecast on same chart
plot_df2 <- plot_df |>
  pivot_longer(
    cols = c(actual_values, baseline_forecast),
    names_to = "type",
    values_to = "value"
  )

# plot traces of actual vs baseline
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

# plot diffs
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
plot_df |>
  ggplot(aes(x = diff)) +
  geom_histogram(color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Difference between Prediction and Actual",
       y = "Count",
       title = "Error Distributions")

## Calculate Overall Baseline MAE & MAPE
baseline_mae <- mean(plot_df$abs_diff)
baseline_absmax <- max(plot_df$abs_diff)
baseline_absmin <- min(plot_df$abs_diff)
baseline_p95 <- quantile(plot_df$abs_diff, 0.95)
baseline_mape <- mean(plot_df$p_error)


## Baseline MAE & MAPE by day of the week
dow_performance <- plot_df |>
  group_by(weekday_name) |>
  summarise(
    mae = mean(abs_diff),
    mape = mean(p_error)
  )

dow1 <- dow_performance |>
  ggplot(aes(x = weekday_name, y = mae)) +
  geom_col() +
  geom_hline(yintercept = baseline_mae, linetype = "dashed") +
  labs(
    x = NULL,
    y = "MAE"
  )

dow2 <- dow_performance |>
  ggplot(aes(x = weekday_name, y = mape)) +
  geom_col() +
  geom_hline(yintercept = baseline_mape, linetype = "dashed") +
  scale_y_continuous(labels = percent) +   # displays as percentage
  labs(
    x = NULL,
    y = "MAPE"
  )

(dow1 + dow2) + plot_annotation(title = "MAE and MAPE by day of the week",
                                subtitle = "Dashed lines show overall average")
    