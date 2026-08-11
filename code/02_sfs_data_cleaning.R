# DATA CLEANING PIPELINE

# PURPOSE: Load sign radar data, join with message randomziation list, create analysis variables, and add weather data.
# INPUTS: sign speed data csv and message randomization list
# OUTPUTS: In-memory objects (full_df + subsets) and CSV exports in data/speed data/subsets/

library(tidyverse) 
library(data.table)
library(openmeteo)

# Load message assignments and data
msg_dir <- file.path("data", "message randomization")

pilot_dates <- read_csv(file.path(msg_dir, "MBT_sign_message_randomization_full_pilot.csv"),
                        show_col_types = FALSE) |> as.data.frame()
ri_ave <- read_csv("data/speed data/sign_ri_ave_through_2026-01-04_1908.csv", 
                   show_col_types = FALSE) 
tanner_park <- read_csv("data/speed data/sign_tanner_park_through_2026-01-05_1404.csv", 
                        show_col_types = FALSE)

# Combine locations and standardize fields (time, location_id, speed, traveler type).
full_df <- bind_rows(ri_ave, tanner_park) |>
  # re-factor traveler type from slow -> fast
  mutate(vehicle_type = factor(traveler_type, levels = c("pedestrian", 
                                                         "bicycle", 
                                                         "electric_vehicle")), 
         # make location names easier to read
         location_id = case_when(location_id %in% "gold-dragon" ~ "tanner_park",
                                 location_id %in% "gold-goose" ~ "ri_ave",
                                 TRUE ~ NA_character_), 
         speed = as.numeric(speed),
         # clean date and time variables
         timestamp = ymd_hms(timestamp, tz = "UTC"),
         timestamp = with_tz(timestamp, tzone = "America/New_York"),
         date = as_date(timestamp),
         hour = hour(timestamp),
         time = format(timestamp, "%H:%M"),
         weekday = lubridate::wday(timestamp, label = TRUE, abbr = FALSE, week_start = 1),
         week = isoweek(timestamp))|>
  # create flags for whether an observation is recorded on a weekend, pilot stage and if user is traveling over certain speed thresholds
  mutate(is_weekend = ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0),
         is_speeding = ifelse(speed > 15, 1, 0),
         over_20 = ifelse(speed > 20, 1, 0),
         over_25 = ifelse(speed > 25, 1, 0),
         over_30 = ifelse(speed > 30, 1, 0),
         pilot_stage = ifelse(date > as.Date("2025-11-10"), 1, 0)) |>
  # remove observations that are speed of zero (people standing) and negative speeds (observations traveling in the opposite direction)
  filter(speed > 1) |>
  # keep only 2 full weeks prior to sign activation
  filter(date >= "2025-10-27" & date <= "2026-01-04") |>
  # keep only sign on hours
  filter(time >= "07:00" & time <= "19:00") |>
  # remove empty weather variables from raw data pull
  select(-weather_temp_c, -weather_precip_mm, 
         -weather_wind_kmh, -weather_cloud_pct,
         -weather_humidity_pct, -weather_visibility_m, 
         -weather_uv_index, -weather_code)

# Join with pilot_dates to get message assignment
full_df <- full_df |>
  left_join(pilot_dates, by = "date") |>
  mutate(message = case_when(location_id == "tanner_park" ~ tanner_park_sign,
                             location_id == "ri_ave" ~ ri_ave_sign,
                             TRUE ~ NA_character_),
         message = ifelse(is.na(message), "none", message),
         message = fct_relevel(message, "none", "text", "image"))|>
  select(-tanner_park_sign, -ri_ave_sign)

# Add hourly weather controls (temperature, precipitation, wind).
weather_data <- weather_history(location = c(38.916, -76.998), # coordinates equidistant between signs
                                # start and end dates of df
                                start = as.character(min(full_df$date, na.rm = TRUE)),
                                end = as.character(max(full_df$date, na.rm = TRUE)),
                                hourly = c("temperature_2m", "precipitation", "windspeed_10m"),
                                timezone = "America/New_York")

hourly_weather <- weather_data |>
  mutate(date = as_date(datetime),
         hour = hour(datetime),
         temp_c = hourly_temperature_2m,
         temp_f = temp_c * 9/5 + 32,
         precip_mm = hourly_precipitation,
         wind_mph_10 = hourly_windspeed_10m) |>
  select(date, hour, temp_f, precip_mm, wind_mph_10)

# Join weather data to full_df
full_df <- full_df |>
  left_join(hourly_weather, by = c("date", "hour"))

# Add daily sunrise/sunset and create a light condition factor based on civil twilight
sunrise_data <- weather_history(location = c(38.916, -76.998),
                                start = as.character(min(full_df$date, na.rm = TRUE)),
                                end = as.character(max(full_df$date, na.rm = TRUE)),
                                daily = c("sunrise", "sunset"),
                                timezone = "America/New_York")

sunrise_sunset_df <- sunrise_data |>
  mutate(sunrise = ymd_hm(daily_sunrise, tz = "America/New_York"),
         sunset = ymd_hm(daily_sunset, tz = "America/New_York")) |>
  select(date, sunrise, sunset)

# Join sun data to full_df
full_df <- full_df |>
  left_join(sunrise_sunset_df, by = "date") |>
  mutate(minutes_since_sunrise = round(as.numeric(difftime(timestamp, sunrise, units = "mins"))),
         minutes_to_sunset = round(as.numeric(difftime(sunset, timestamp, units = "mins")))) |>
  mutate(
    # Classify based on minutes since sunrise / to sunset
    light_factor = case_when(
      # TWILIGHT: within 30 min window before sunrise OR within window after sunset
      (minutes_since_sunrise > -30 & minutes_since_sunrise <= 0) |
        (minutes_to_sunset < 0 & minutes_to_sunset >= -30) ~ "twilight",
      # FULL SUN: strictly after sunrise and strictly before sunset
      (minutes_since_sunrise > 0 & minutes_to_sunset > 0) ~ "full_sun",
      # DARK: no sunlight
      TRUE ~ "dark"
    ),
    light_factor = factor(light_factor, levels = c("full_sun", "twilight", "dark"))
  ) |>
  select(-minutes_since_sunrise, -minutes_to_sunset)

# Create trailing 5-minute volume measure + percent pedestrian (share in prior 5 minutes)
{
  full_df <- as.data.table(full_df)
  setkey(full_df, location_id, timestamp) # ensure sorted for joins
  full_df[, window_start := timestamp - 300] # 5 min (300s) rolling window start
  
  # Total volume in prior 5 minutes (including current obs)
  full_df[, volume_5mins :=
            full_df[.SD,
                    on = .(location_id,
                           timestamp > window_start,
                           timestamp <= timestamp),
                    .N,
                    by = .EACHI]$N
  ]
  
  # Pedestrian volume in prior 5 minutes
  full_df[, ped_5mins :=
            full_df[.SD,
                    on = .(location_id,
                           timestamp > window_start,
                           timestamp <= timestamp),
                    sum(traveler_type == "pedestrian", na.rm = TRUE),
                    by = .EACHI]$V1
  ]
  
  # Percent pedestrian in the rolling window
  full_df[, pct_ped_5mins := fifelse(volume_5mins > 0, ped_5mins / volume_5mins, NA_real_)]
  full_df[, window_start := NULL] # remove temporary column
  invisible(NULL)
  }

full_df <- full_df|>
  mutate(
    week_rel_intervention = as.integer(floor(as.numeric(date - as.Date("2025-11-10")) / 7)),
    week_rel_intervention = dplyr::if_else(
      week_rel_intervention < 0L,
      week_rel_intervention,        # keep baseline as -2, -1
      week_rel_intervention + 1L),    # shift intervention weeks to start at 1 instead of zero
    week_rel_factor = factor(week_rel_intervention, levels = c(-2, -1, 1:8)) |>
      relevel(ref = "-1"))
