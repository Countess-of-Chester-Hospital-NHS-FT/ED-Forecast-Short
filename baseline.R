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
         weekday_name = wday(check_in_date_time, label = TRUE, abbr = TRUE, week_start = 1),
         year = year(check_in_date_time)
  ) |>
  filter(check_in_date >= ymd("2022-11-01"))

saveRDS(data, "data.RDS")

missing_data <- missing_glimpse(data)

### Plot of daily attends
plot_df <- data |>
  count(check_in_date, weekday_name)|>
  arrange(check_in_date, weekday_name) |> 
  mutate(
    rolling_12wk_avg = zoo::rollmean(n, k = 12, fill = NA, align = "right")
  )

