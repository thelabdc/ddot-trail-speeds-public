library(tidyverse)
library(lubridate)
library(hms)
library(openmeteo)
library(broom)


intervention_start <- as.Date("2025-11-10")

tz_local <- "America/New_York"
counter_file <- "data/speed data/counter_tanner_IN_oct_23_feb_20.csv"

wx_coords <- c(38.916, -76.998)
wx_start <- "2025-10-23"
wx_end   <- "2026-02-20"

# Data sources
# - Downstream: Eco-Counter 15-minute bins (inbound)
# - Upstream: Radar data at the Tanner speed-feedback sign (full_df)

# Downstream bin
downstream_bin <- read_csv(counter_file, show_col_types = FALSE) %>%
  mutate(
    bin_15min = mdy_hm(Time, tz = tz_local),
    source = "Tanner_Counter"
  ) %>%
  rename(
    speed_1_6mph = `MBT - Alethia Tanner Park IN 1 - 6 mph`,
    speed_7_14_mph = `MBT - Alethia Tanner Park IN 7 - 14 mph`,
    speed_15_21_mph = `MBT - Alethia Tanner Park IN 15 - 21 mph`,
    speed_22_28_mph = `MBT - Alethia Tanner Park IN 22 - 28 mph`,
    speed_29_35mph = `MBT - Alethia Tanner Park IN 29 - 35 mph`,
    speed_35_plus = `MBT - Alethia Tanner Park IN  > 35 mph`
  ) %>%
  mutate(across(starts_with("speed_"), ~ ifelse(is.na(.x), 0, .x))) %>%
  
  # Keep pedestrian bin temporarily so we can use it for total counts + shares
  mutate(
    n_obs_total = speed_1_6mph + speed_7_14_mph + speed_15_21_mph +
      speed_22_28_mph + speed_29_35mph + speed_35_plus,
    ped_share   = ifelse(n_obs_total > 0, speed_1_6mph / n_obs_total, NA)
  ) %>%
  
  # Now drop pedestrians for avg speed calc + downstream alignment
  select(bin_15min, source, ped_share, n_obs_total,
         speed_7_14_mph, speed_15_21_mph, speed_22_28_mph,
         speed_29_35mph, speed_35_plus) %>%
  
  mutate(
    n_obs = speed_7_14_mph + speed_15_21_mph + speed_22_28_mph +
      speed_29_35mph + speed_35_plus,
    
    avg_speed = (11 * speed_7_14_mph +
                   18 * speed_15_21_mph +
                   25 * speed_22_28_mph +
                   32 * speed_29_35mph +
                   35 * speed_35_plus) / n_obs,
    pct_above_22mph = (speed_22_28_mph + speed_29_35mph + speed_35_plus)/n_obs # As for average speed, we want to take this as a share of the non-pedestrians
  ) |>
  arrange(bin_15min)

# Upstream bin 
upstream_bin <- full_df %>%
  filter(location_id == "tanner_park") %>%
  mutate(
    timestamp = ymd_hms(timestamp, tz = tz_local),
    bin_15min = floor_date(timestamp, "15 minutes")
  ) %>%
  mutate(
    speed_1_6mph    = ifelse(speed >= 1 & speed < 7, 1, 0),
    speed_7_14_mph  = ifelse(speed >= 7 & speed < 15, 1, 0),
    speed_15_21_mph = ifelse(speed >= 15 & speed < 22, 1, 0),
    speed_22_28_mph = ifelse(speed >= 22 & speed < 28, 1, 0),
    speed_29_35mph  = ifelse(speed >= 28 & speed < 35, 1, 0),
    speed_35_plus   = ifelse(speed > 35, 1, 0)
  ) %>%
  group_by(bin_15min) %>%
  summarise(
    # total obs including pedestrians
    n_obs_total = n(),
    
    across(starts_with("speed_"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    # pedestrian share: pedestrian bin / total obs
    ped_share = ifelse(n_obs_total > 0, speed_1_6mph / n_obs_total, NA)
  ) %>%
  # drop pedestrian bin now for avg speed consistency
  select(bin_15min, ped_share, n_obs_total, source = NULL,
         speed_7_14_mph, speed_15_21_mph, speed_22_28_mph,
         speed_29_35mph, speed_35_plus) %>%
  mutate(
    # n_obs excluding pedestrians (for compatibility with downstream logic)
    n_obs = speed_7_14_mph + speed_15_21_mph + speed_22_28_mph +
      speed_29_35mph + speed_35_plus,
    
    avg_speed = (11 * speed_7_14_mph +
                   18 * speed_15_21_mph +
                   25 * speed_22_28_mph +
                   32 * speed_29_35mph +
                   35 * speed_35_plus) / n_obs,
    pct_above_22mph = (speed_22_28_mph + speed_29_35mph + speed_35_plus)/n_obs, # As for average speed, we want to take this as a share of the non-pedestrians
    source = "Tanner_SFS"
  ) %>%
  arrange(bin_15min)

# Keep 07:00–19:00, but only retain 15-min bins where BOTH sources have data.
time_window_start <- as_hms("07:00:00")
time_window_end   <- as_hms("19:00:00")

common_bins <- inner_join(
  upstream_bin |> distinct(bin_15min),
  downstream_bin |> distinct(bin_15min),
  by = "bin_15min"
) |>
  mutate(tod = as_hms(bin_15min)) |>
  filter(tod >= time_window_start, tod <= time_window_end) |>
  select(-tod)

upstream_bin <- upstream_bin |>
  filter(bin_15min %in% common_bins$bin_15min)

downstream_bin <- downstream_bin |>
  filter(bin_15min %in% common_bins$bin_15min)

weather_hourly <- weather_history(
  location = wx_coords,
  start = wx_start,
  end = wx_end,
  hourly = list("temperature_2m", "precipitation", "windspeed_10m"),
  timezone = tz_local,
) |>
  mutate(
    hour = hour(datetime),
    date = as.Date(datetime),
    temp_c = hourly_temperature_2m,
    temp_f = (temp_c * (9 / 5)) + 32
  ) |>
  transmute(
    date,
    hour,
    temp_f,
    precip_mm = hourly_precipitation,
    wind_mph_10 = hourly_windspeed_10m
  )

weather_daily <- weather_history(
  location = c(38.89837931807914, -77.0061148404401),
  start = wx_start,
  end = wx_end,
  daily = list("sunrise", "sunset"),
  timezone = tz_local,
) |>
  mutate(
    date = as.Date(date),
    daily_sunset = as.POSIXct(daily_sunset, format = "%Y-%m-%dT%H:%M", tz = tz_local),
    daily_sunrise = as.POSIXct(daily_sunrise, format = "%Y-%m-%dT%H:%M", tz = tz_local)
  ) |>
  select(date, daily_sunrise, daily_sunset)

did_long <- bind_rows(
  upstream_bin |>
    transmute(
      bin_15min,
      source,
      n_obs_total,
      ped_share,
      avg_speed,
      pct_above_22mph
    ),
  downstream_bin |>
    transmute(
      bin_15min,
      source,
      n_obs_total,
      ped_share,
      avg_speed,
      pct_above_22mph
    )
) |>
  mutate(
    hour_of_week = paste(wday(bin_15min), hour(bin_15min), sep = " "),
    hour = hour(bin_15min),
    date = as.Date(bin_15min)
  ) |>
  left_join(weather_hourly, by = c("hour", "date")) |>
  left_join(weather_daily, by = "date") |>
  mutate(
    mins_before_sunrise = as.numeric(difftime(daily_sunrise, bin_15min, units = "mins")),
    mins_after_sunset   = as.numeric(difftime(bin_15min, daily_sunset, units = "mins")),
    light_factor = case_when(
      bin_15min >= daily_sunrise & bin_15min <= daily_sunset ~ "sunlight",
      (mins_before_sunrise >= 0 & mins_before_sunrise <= 30) |
        (mins_after_sunset >= 0 & mins_after_sunset <= 30) ~ "twilight",
      (mins_before_sunrise > 30) | (mins_after_sunset > 30) ~ "dark",
      TRUE ~ "dark"
    ),
    light_factor = factor(light_factor, levels = c("sunlight", "twilight", "dark")),
    post = ifelse(date >= intervention_start, 1, 0)
  )


# Create a complete 15-minute interval dataset for BOTH sensors. "Missing" counts are relabeled as 0 and ensure no gaps.

did_long <- did_long |>
  right_join(
    expand_grid(
      bin_15min = seq(
        from = floor_date(as.POSIXct("2025-10-27 00:00", tz = tz_local)),
        to   = floor_date(as.POSIXct("2026-01-05 00:00", tz = tz_local)),
        by   = "15 mins"
      ),
      source = c("Tanner_SFS", "Tanner_Counter")
    ),
    by = c("source", "bin_15min")
  ) |>
  mutate(
    n_obs_total = ifelse(is.na(n_obs_total), 0, n_obs_total),
    date = as.Date(bin_15min),
    hour = hour(bin_15min),
    hour_of_week = paste(wday(bin_15min), hour(bin_15min), sep = " "),
    post = ifelse(date >= intervention_start, 1, 0)
  ) |>
  group_by(source) |>
  arrange(bin_15min) |>
  mutate(vol_15_min = lag(n_obs_total, 1),
         ped_share_15_min = lag(ped_share,1)) |>
  ungroup() |>
  filter(bin_15min %in% common_bins$bin_15min) |>
  mutate(
    source = factor(source, levels = c("Tanner_SFS", "Tanner_Counter")),
    light_factor = droplevels(light_factor),
    ped_share_15_min = ifelse(is.na(ped_share_15_min),0, ped_share_15_min) # Don't want to drop those observations just because in the 15 min prior there were no pedestrians
  )

# Create a long-format dataset for just the counter (for downstream event studies) 
long_data_counter <- 
  did_long |> 
  filter(source == "Tanner_Counter")|>
  filter(as_hms(bin_15min) >= as_hms("07:00:00") & as_hms(bin_15min) <= as_hms("19:00:00"))|>
  mutate(wday = wday(bin_15min))|>
  arrange(bin_15min)|>
  mutate(week = isoweek(bin_15min)) |> 
  mutate(weeks_rel_intervention = case_when(
    date < as.Date("2025-11-10") ~ week - isoweek(as.Date("2025-11-10")),
    (date > as.Date("2025-11-10") & date < as.Date("2025-12-29")) ~  (week - isoweek(as.Date("2025-11-10")) + 1),
    date > as.Date("2025-12-29") ~ (week + 7)
  ))|>
  mutate(weeks_rel_intervention_factor = factor(weeks_rel_intervention, levels = c(-1,-2,1,2,3,4,5,6,7,8)))

  
  
