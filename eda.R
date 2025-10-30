library(tidyverse)
library(janitor)
library(finalfit)
library(zoo)

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
         weekday_name = wday(check_in_date_time, label = TRUE, abbr = FALSE, week_start = 1),
         year = year(check_in_date_time)
         ) |>
  filter(check_in_week > ymd("2017-04-02"),
         check_in_week < floor_date(now(), "week", week_start = 1))

test <- data |>
  distinct(check_in_date, iso_week_number, iso_year) |>
  group_by(iso_week_number, iso_year) |>
  count() |>
  filter(n != 7)

missing_data <- missing_glimpse(data)

### Global variables
covid_start <- floor_date(ymd("2020-01-01"), unit = "week", week_start = 1)
covid_end   <- floor_date(ymd("2023-01-01"), unit = "week", week_start = 1)
cerner_start <- ymd("2021-07-23")

### Long term trend since 2017
plot_df <- data |>
  count(check_in_week)|>
  arrange(check_in_week) |>  # ensure data is in order
  mutate(
    rolling_6wk_avg = zoo::rollmean(n, k = 6, fill = NA, align = "right"),
    covid_period = if_else(check_in_week %within% (covid_start%--%covid_end),
                           TRUE, FALSE)
  )

plot_df |>
  ggplot(aes(x = check_in_week, y = n)) +
  geom_rect(aes(xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.5, inherit.aes = FALSE) +
  geom_vline(xintercept = cerner_start, color = "black", linetype = "dashed") +
  geom_line() +
  #geom_smooth(method = "lm") +
  geom_line(aes(y = rolling_6wk_avg), color = "orange", size = 1.2, linetype = "solid") +
  labs(title = "Weekly ED Attendances since April 2017",
       x = "Week",
       y = "Attendances")

### Can we compare this to the national trends / other similar trusts?

## Not easily without import project

### Yearly seasonality

## All data with covid chopped out

data_nocovid <- data |>
  filter(!check_in_week %within% (covid_start%--%covid_end)) #|>
  #filter(iso_year == 2025,
         #iso_week_number == 1)

year_plot_df <- data_nocovid |>
  count(iso_week_number, iso_year) |>
  arrange(iso_week_number)

# Split data
other_years_df <- year_plot_df |> filter(iso_year != 2025)
y2025_df      <- year_plot_df |> filter(iso_year == 2025)

# Calculate average across years for each week
average_week_df <- other_years_df %>%
  group_by(iso_week_number) %>%
  summarize(avg_n = mean(n), .groups = 'drop')

bst_start <- 12
bst_end <- 43

season_split_df <- other_years_df |>
  mutate(summer = if_else(between(iso_week_number, bst_start, bst_end), TRUE, FALSE)) |>
  group_by(summer) |>
  summarise(mean_n = mean(n, na.rm = TRUE))

summer_mean <- season_split_df |> filter(summer) |> pull(mean_n)
winter_mean <- season_split_df |> filter(!summer) |> pull(mean_n)

meteo_spring <- 9-22
meteo_summer <- 23-35
meteo_autumn <- 36-48
meteo_winter <- 49-8

spring_ht <- 8
easter_hols_start <- 13.5
easter_hols_end <- 15.5
summer_ht <- 22.5
summer_hols_start <- 29.5
summer_hols_end <- 35.5
autumn_ht <- 43
xmas_hols_start <- 51
xmas_hols_end <- 1.5

## Plot y on y + average
other_years_df |>
  ggplot(aes(x = iso_week_number, y = n, color = as_factor(iso_year))) +
  geom_rect(xmin = bst_start, xmax = bst_end, ymin = -Inf, ymax = Inf,
            fill = "lightyellow1", alpha = 0.045, inherit.aes = FALSE) +
  geom_rect(xmin = summer_hols_start, xmax = summer_hols_end, ymin = -Inf, ymax = Inf,
            fill = "gold1", alpha = 0.002, inherit.aes = FALSE) +
  geom_rect(xmin = easter_hols_start, xmax = easter_hols_end, ymin = -Inf, ymax = Inf,
            fill = "gold1", alpha = 0.002, inherit.aes = FALSE) +
  geom_rect(xmin = xmas_hols_start, xmax = 52, ymin = -Inf, ymax = Inf,
            fill = "gold1", alpha = 0.002, inherit.aes = FALSE) +
  geom_rect(xmin = 1, xmax = xmas_hols_end, ymin = -Inf, ymax = Inf,
            fill = "gold1", alpha = 0.002, inherit.aes = FALSE) +
  geom_rect(xmin = spring_ht-0.5, xmax = spring_ht+0.5, ymin = -Inf, ymax = Inf,
            fill = "gold1", alpha = 0.002, inherit.aes = FALSE) +
  geom_rect(xmin = summer_ht-0.5, xmax = summer_ht+0.5, ymin = -Inf, ymax = Inf,
            fill = "gold1", alpha = 0.002, inherit.aes = FALSE) +
  geom_rect(xmin = autumn_ht-0.5, xmax = autumn_ht+0.5, ymin = -Inf, ymax = Inf,
            fill = "gold1", alpha = 0.002, inherit.aes = FALSE) +
  geom_line() +
  geom_point(size = 1) +
  # 2025 highlighted layer
  geom_line(data = y2025_df, aes(x = iso_week_number, y = n, color = as_factor(iso_year)),  linewidth = 1) +
  geom_point(data = y2025_df, aes(x = iso_week_number, y = n, color = as_factor(iso_year)), size = 1.5) +
  geom_line(data = average_week_df, aes(x = iso_week_number, y = avg_n),
            color = "black", size = 1.2, linetype = "solid") +
  annotate("label",
           x = 25,
           y = 1900,
           label = paste("Summer:", round(summer_mean))) +
  annotate("label",
           x = 5,
           y = 1900,
           label = paste("Winter:", round(winter_mean))) +
  labs(title = "ED Attendances by Week of the Year",
       subtitle = "Yellow shading indicates BST and approx. school holidays",
       x = "Week Number (ISO 8601)",
       y = "Attendances") +
  theme(legend.title = element_blank())

