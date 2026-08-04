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
prophet <- read_rds("prophet_12monthly_average.RDS") |>
  mutate(
    method = "prophet"
  )

prophet2 <- read_rds("prophet_12monthly_average2.RDS") |>
  mutate(
    method = "prophet2"
  )

arima_csv <- read_csv("arima_12month.csv", col_names = F)

arima <- arima_csv |>
  rename(
    month = X1,
    average_daily_central = X2,
    average_daily_lower = X3,
    average_daily_upper = X4
  ) |>
  mutate(
    method = "arima"
  )

compare_df <- prophet |>
  bind_rows(arima) |>
  bind_rows(prophet2) |>
  select(
    month,
    average_daily_central,
    average_daily_lower,
    average_daily_upper,
    method
  )

### combined plot
compare_df |>
  filter(
    method != "prophet"
  ) |>
  ggplot(
    aes(
      x = month,
      y = average_daily_central,
      colour = method,
      fill = method,
      group = method
    )
  ) +
  geom_ribbon(
    aes(
      ymin = average_daily_lower,
      ymax = average_daily_upper
    ),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c(
      "arima" = "#3182bd",
      "prophet2" = "#e6550d"
    )
  ) +
  scale_fill_manual(
    values = c(
      "arima" = "#3182bd",
      "prophet2" = "#e6550d"
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Comparison of monthly attendance forecasts",
    subtitle = "Shaded areas show prediction intervals",
    x = NULL,
    y = "Predicted attendances",
    colour = "Forecast method",
    fill = "Forecast method"
  )

### plot

prophet |>
  ggplot(aes(x = month, y = average_daily_central)) +
  geom_ribbon(
    aes(ymin = average_daily_lower, ymax = average_daily_upper),
    fill = "#3182bd",
    alpha = 0.25
  ) +
  geom_line() +
  geom_point() +
  # scale_x_date(
  #   breaks = seq(
  #     from = ymd("260801"),
  #     to   = ymd("260930"),
  #     by   = "3 days"
  #   ),
  #   date_labels = "%d %b",
  #) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Prophet Monthly forecast of attendances",
    subtitle = "<span style='color:#3182bd'>■</span> 80% Prediction Intervals",
    x = NULL,
    y = "Predicted Attendances"
  )+
  theme(plot.subtitle = element_markdown())

arima |>
  ggplot(aes(x = month, y = average_daily_central)) +
  geom_ribbon(
    aes(ymin = average_daily_lower, ymax = average_daily_upper),
    fill = "#3182bd",
    alpha = 0.25
  ) +
  geom_line() +
  geom_point() +
  # scale_x_date(
  #   breaks = seq(
  #     from = ymd("260801"),
  #     to   = ymd("260930"),
  #     by   = "3 days"
  #   ),
  #   date_labels = "%d %b",
  #) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "ARIMA Monthly forecast of attendances",
    subtitle = "<span style='color:#3182bd'>■</span> 80% Prediction Intervals",
    x = NULL,
    y = "Predicted Attendances"
  )+
  theme(plot.subtitle = element_markdown())
