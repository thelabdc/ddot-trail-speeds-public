# DESCRIPTIVE ANALYSES

# PURPOSE: Baseline (pre-intervention) diagnostics and descriptive plots
# INPUTS: baseline dataframe and over_7mph (subsetted from full_df)
# OUTPUTS: Saves figures to /figs/ 

library(tidyverse)
library(tidyr)
library(readr)
library(scales)
library(knitr)

# baseline only observations
baseline <- full_df |> 
  filter(date < as.Date("2025-11-10")) 

# speeds over 7 mph only
over_7mph <- full_df |> 
  filter(speed >= 7) 

# speeds over 7 mph only, between 8-9:30 am morning rush
over_7mph_AM <- full_df |> 
  filter(speed >= 7) |>
  filter((hour == 8) | (hour == 9 & minute(timestamp) < 30))

# start and end dates for each pilot week
week_date_map <- full_df |>
  group_by(location_id, week_rel_factor) |>
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    n = n(),
    .groups = "drop") |>
  arrange(as.numeric(as.character(week_rel_factor)))

# Use calendar dates (M/DD) as x-axis labels instead of week indices
week_labels <- week_date_map |>
  group_by(week_rel_factor) |>
  summarise(start_date = min(start_date, na.rm = TRUE), .groups = "drop") |>
  arrange(as.numeric(as.character(week_rel_factor))) |>
  mutate(
    week = as.numeric(as.character(week_rel_factor)),
    label = format(start_date, "%m/%d")
  )

x_breaks <- week_labels$week
x_labels <- week_labels$label


# Identify missing hourly coverage by location and day

status <- full_df |>
  distinct(location_id, date, hour) |>
  filter(hour %in% 7:18) |>
  mutate(has_data = TRUE) |>
  complete(
    location_id,
    date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "day"),
    hour = 7:18
  ) |>
  replace_na(list(has_data = FALSE)) |>
  pivot_wider(names_from = location_id, values_from = has_data, values_fill = FALSE) |>
  mutate(missing_status = case_when(
    !ri_ave &  tanner_park ~ "Missing: RI Ave",
    ri_ave & !tanner_park ~ "Missing: Alethia Tanner",
    !ri_ave & !tanner_park ~ "Completely Missing",
    TRUE                    ~ "Present"
  ))

# Plot missingness heatmap
boundary_date <- as.Date("2025-11-09") + 0.5  

missing_data <- ggplot(status, aes(x = hour, y = date)) +
  geom_tile(aes(fill = missing_status), color = "white", linewidth = 0.3) +
  geom_hline(yintercept = boundary_date, color = "gray10", linewidth = 0.8) +
  scale_fill_manual(values = c(
    "Missing: RI Ave" = "red4",
    "Missing: Alethia Tanner" = "blue4",
    "Completely Missing" = "black",
    "Present" = "gray"
  )) +
  scale_x_reverse(
    breaks = 7:18,
    labels = c("7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm"),
    expand = c(0, 0)
  ) +
  scale_y_date(date_labels = "%b %d", date_breaks = "1 day", expand = c(0, 0)) +
  labs(
    title = "Missing Hourly Data by Location",
    x = "Hour of Day",
    y = "Date",
    fill = "Data Status"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  coord_flip() +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

# Average users for each day of the week by location (baseline)
plot_data <- baseline |>
  group_by(date, weekday, location_id) |>
  summarise(daily_users = n(), .groups = "drop") |>
  group_by(weekday, location_id) |>
  summarise(avg_users = mean(daily_users), .groups = "drop")

avg_users_plot <- ggplot(plot_data, aes(x = weekday, y = avg_users, fill = location_id)) +
  geom_col() +
  geom_text(aes(label = round(avg_users, 0)), vjust = -0.5, size = 4) +
  facet_grid(~ location_id) +
  scale_fill_manual(values = c(
    ri_ave = "firebrick",
    tanner_park = "steelblue"
  )) +
  labs(
    title = "Average Number of Users per Weekday by Location",
    x = "Weekday",
    y = "Average Daily Users"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Hourly Traffic Volume, Faceted by Weekday/end (Speeding vs Not Speeding)

plot_data <- baseline |>
  filter(hour %in% 7:18) |>
  group_by(is_weekend, date, hour) |>
  summarise(
    speeding = sum(is_speeding, na.rm = TRUE),
    not_speeding = sum(!is_speeding, na.rm = TRUE),
    .groups = "drop") |>
  group_by(is_weekend, hour) |>
  summarise(across(c(speeding, not_speeding), mean), .groups = "drop") |>
  tidyr::complete(is_weekend, hour = 7:18, fill = list(speeding = 0, not_speeding = 0)) |>
  mutate(
    period = if_else(is_weekend == 1, "Weekend", "Weekday"),
    hour = factor(hour, levels = 7:18, ordered = TRUE)) |>
  pivot_longer(c(not_speeding, speeding), names_to = "type", values_to = "count")

plot_weekday_weekend <- ggplot(plot_data, aes(hour, count, fill = type)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_discrete(labels = c("7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm"), drop = FALSE) +
  scale_fill_manual(values = c(not_speeding = "steelblue", speeding = "firebrick")) +
  facet_grid(~ period) +
  labs(
    title = "Average Hourly Traffic Volume and Speeding: Weekdays vs Weekends",
    x = "Hour of Day",
    y = "Average User Count",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Baseline speeds by location

baseline_speed_bins <- baseline |>
  mutate(
    speed_bin = case_when(
      speed >= 0  & speed < 5  ~ "0–5 mph",
      speed >= 5  & speed < 10 ~ "5–10 mph",
      speed >= 10 & speed < 15 ~ "10–15 mph",
      speed >= 15 & speed < 20 ~ "15–20 mph",
      speed >= 20 & speed < 25 ~ "20–25 mph",
      speed >= 25 & speed < 30 ~ "25–30 mph",
      speed >= 30              ~ "30+ mph",
      TRUE ~ NA_character_
    ),
    speed_bin = factor(
      speed_bin,
      levels = c("0–5 mph", "5–10 mph", "10–15 mph", "15–20 mph", "20–25 mph", "25–30 mph", "30+ mph")
    ),
    location_label = case_when(
      location_id == "tanner_park" ~ "Alethia Tanner Park",
      location_id == "ri_ave" ~ "Rhode Island Ave",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(speed_bin), !is.na(location_label)) |>
  group_by(location_label) |>
  mutate(location_n = n()) |>
  group_by(location_label, location_n, speed_bin) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(
    pct = n / location_n,
    legend_label = paste0(location_label, " (n = ", comma(location_n), ")")
  )

baseline_speeds_by_location_plot <- ggplot(
  baseline_speed_bins,
  aes(x = speed_bin, y = pct, fill = legend_label)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 0.35)
  ) +
  scale_fill_manual(
    values = c(
      "Alethia Tanner Park (n = 19,297)" = "steelblue",
      "Rhode Island Ave (n = 9,545)" = "firebrick"
    )
  ) +
  labs(
    title = "Baseline speeds by location",
    x = NULL,
    y = "Percent of observations",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

speed_phase_plot_data <- full_df |>
  mutate(
    phase = if_else(date < as.Date("2025-11-10"), "Baseline", "Intervention"),
    location_label = case_when(
      location_id == "ri_ave" ~ "RI Ave",
      location_id == "tanner_park" ~ "Tanner Park",
      TRUE ~ NA_character_
    ),
    speed_bin = case_when(
      speed >= 0  & speed < 5  ~ "0–5",
      speed >= 5  & speed < 10 ~ "5–10",
      speed >= 10 & speed < 15 ~ "10–15",
      speed >= 15 & speed < 20 ~ "15–20",
      speed >= 20 & speed < 25 ~ "20–25",
      speed >= 25              ~ "25+",
      TRUE ~ NA_character_
    ),
    speed_bin = factor(
      speed_bin,
      levels = c("0–5", "5–10", "10–15", "15–20", "20–25", "25+")
    )
  ) |>
  filter(!is.na(location_label), !is.na(speed_bin)) |>
  group_by(location_label, phase) |>
  mutate(group_n = n()) |>
  group_by(location_label, phase, group_n, speed_bin) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(
    pct = n / group_n,
    legend_label = paste0(location_label, " — ", phase, " (n = ", scales::comma(group_n), ")"),
    legend_label = factor(
      legend_label,
      levels = c(
        "RI Ave — Baseline (n = 9,545)",
        "RI Ave — Intervention (n = 32,542)",
        "Tanner Park — Baseline (n = 19,297)",
        "Tanner Park — Intervention (n = 36,050)"
      )
    )
  )

speed_phase_plot <- ggplot(
  speed_phase_plot_data,
  aes(x = speed_bin, y = pct, fill = legend_label)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 5
  ) +
  facet_wrap(~ location_label) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.38)
  ) +
  scale_fill_manual(
    values = c(
      "RI Ave — Baseline (n = 9,545)" = "#C44E52",
      "RI Ave — Intervention (n = 32,542)" = "#E88C8C",
      "Tanner Park — Baseline (n = 19,297)" = "#4C78A8",
      "Tanner Park — Intervention (n = 36,050)" = "#8FBCE6"
    )
  ) +
  labs(
    title = "Speeds by location: baseline vs intervention",
    x = "Speed bin",
    y = "Percent of observations",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top",
    legend.direction = "horizontal",
    strip.text = element_text(size = 14, face = "bold")
  )

# summary stats for each location
summarize_data <- function(data) {
  descriptive_stats <- data |>
    group_by(location_id, vehicle_type) |>
    summarise(
      count = n(),
      min = min(speed, na.rm = TRUE),
      median = median(speed, na.rm = TRUE),
      mean = round(mean(speed, na.rm = TRUE), 2),
      "85%" = quantile(speed, 0.85, na.rm = TRUE),
      max = max(speed, na.rm = TRUE),
      "% speeding" = round(mean(is_speeding == 1, na.rm = TRUE) * 100, 2),
      .groups = "drop"
    )
  
  total_stats <- data |>
    group_by(location_id) |>
    summarise(
      vehicle_type = "TOTAL",
      count = n(),
      min = min(speed, na.rm = TRUE),
      median = median(speed, na.rm = TRUE),
      mean = round(mean(speed, na.rm = TRUE), 2),
      "85%" = quantile(speed, 0.85, na.rm = TRUE),
      max = max(speed, na.rm = TRUE),
      "% speeding" = round(mean(is_speeding == 1, na.rm = TRUE) * 100, 2),
      .groups = "drop"
    )
  
  bind_rows(descriptive_stats, total_stats) |>
    arrange(location_id, desc(vehicle_type == "TOTAL"), vehicle_type)
}

# count and percent speeding at different thresholds for each location
speed_stats <- function(data, thresholds = c(15, 20, 25, 30)) {
  for (location in unique(data$location_id)) {
    cat(location, ":", sep = "")
    data_location <- filter(data, location_id == location)
    for (x in thresholds) {
      count <- sum(data_location$speed > x, na.rm = TRUE)
      percent <- round((count / nrow(data_location)) * 100, 2)
      cat(count, " users (", percent, "%) are traveling over ", x, " mph\n", sep = "")
    }
  }
  
  invisible(NULL)
}

# Capture outputs as objects for exporting / embedding in Quarto text
speed_threshold_table <- baseline |>
  group_by(location_id) |>
  summarise(
    `>15 mph` = round(mean(speed > 15, na.rm = TRUE) * 100, 2),
    `>20 mph` = round(mean(speed > 20, na.rm = TRUE) * 100, 2),
    `>25 mph` = round(mean(speed > 25, na.rm = TRUE) * 100, 2),
    `>30 mph` = round(mean(speed > 30, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )
baseline_speed_summary_table <- summarize_data(baseline)

# descriptive stats for each location during baseline and intervention

descriptive_by_phase <- over_7mph |>
  mutate(phase = case_when(date < as.Date("2025-11-10") ~ "Baseline",
                           date >= as.Date("2025-11-10") ~ "Intervention")) |>
  group_by(location_id, phase) |>
  summarise(n = n(),
            mean_speed = round(mean(speed, na.rm = TRUE), 2),
            median_speed = round(median(speed, na.rm = TRUE), 2),
            p85_speed = round(quantile(speed, 0.85, na.rm = TRUE), 2),
            max_speed = round(max(speed, na.rm = TRUE), 2),
            pct_speeding = round(mean(is_speeding, na.rm = TRUE) * 100, 2),
            .groups = "drop")

descriptive_by_phase_am <- over_7mph_AM |>
  mutate(phase = case_when(date < as.Date("2025-11-10") ~ "Baseline",
                           date >= as.Date("2025-11-10") ~ "Intervention")) |>
  group_by(location_id, phase) |>
  summarise(n = n(),
            mean_speed = round(mean(speed, na.rm = TRUE), 2),
            median_speed = round(median(speed, na.rm = TRUE), 2),
            p85_speed = round(quantile(speed, 0.85, na.rm = TRUE), 2),
            max_speed = round(max(speed, na.rm = TRUE), 2),
            pct_speeding = round(mean(is_speeding, na.rm = TRUE) * 100, 2),
            .groups = "drop")

# Convert key descriptive tables to markdown for embedding in Quarto text
descriptive_by_phase_md <- knitr::kable(descriptive_by_phase)
descriptive_by_phase_am_md <- knitr::kable(descriptive_by_phase_am)

# PERCENT SPEEDING BY WEEK

speeding <- full_df |>
  group_by(location_id, week_rel_intervention) |>
  summarise(
    n_obs = n(),
    pct_over15 = mean(is_speeding, na.rm = TRUE),
    pct_over20 = mean(over_20, na.rm = TRUE),
    pct_over25 = mean(over_25, na.rm = TRUE),
    pct_over30 = mean(over_30, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(location_id, week_rel_intervention)

pct_speeding_long <- speeding |>
  pivot_longer(
    cols = c(pct_over15, pct_over20, pct_over25, pct_over30),
    names_to = "threshold",
    values_to = "pct"
  ) |>
  mutate(
    threshold = recode(
      threshold,
      pct_over15 = "> 15 mph",
      pct_over20 = "> 20 mph",
      pct_over25 = "> 25 mph",
      pct_over30 = "> 30 mph"
    )
  )

pct_speeding_plot <- ggplot(pct_speeding_long, aes(x = week_rel_intervention, y = pct, color = threshold)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ location_id) +
  scale_x_continuous(
    limits = c(-2, 8),
    breaks = x_breaks,
    labels = x_labels
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, NA)
  ) +
  labs(
    title = "% above speed thresholds over time",
    subtitle = "(Red dashed line = intervention starts)",
    x = "Week",
    y = "Percent of observations",
    color = "Threshold"
  ) +
  theme_minimal()

# UPPER TAIL SPEEDS

tail_by_week <- full_df |>
  group_by(location_id, week_rel_intervention) |>
  summarise(
    p90 = quantile(speed, 0.90, na.rm = TRUE),
    p95 = quantile(speed, 0.95, na.rm = TRUE),
    p99 = quantile(speed, 0.99, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(p90:p99, names_to = "percentile", values_to = "speed") |>
  mutate(
    percentile = recode(
      percentile,
      p90 = "90th",
      p95 = "95th",
      p99 = "99th"
    )
  )

tail_speed_plot <- ggplot(tail_by_week, aes(x = week_rel_intervention, y = speed, color = percentile)) +
  scale_x_continuous(limits = c(-2, 8), breaks = x_breaks, labels = x_labels) +
  scale_y_continuous(limits = c(0, 45)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~ location_id) +
  labs(
    title = "Excessive speeding trends over time",
    subtitle = "Red dashed line = intervention starts",
    x = "Week",
    y = "Speed (mph)",
    color = "Percentile"
  ) +
  theme_minimal()

# pooled speed incidence before and after

Y <- 1000 # rate per Y number of users, change if you want per 100, 500, etc

pooled_incidence <- full_df %>%
  filter(!is.na(speed), !is.na(week_rel_intervention), !is.na(location_id)) %>%
  mutate(
    week_rel_intervention = as.integer(week_rel_intervention),
    period = if_else(week_rel_intervention < 0, "Before", "After")
  ) %>%
  group_by(location_id, period) %>%
  summarise(
    n_weeks = n_distinct(week_rel_intervention),
    pooled_rate_over15 = round(mean(speed > 15) * Y),
    pooled_rate_over20 = mean(speed > 20) * Y,
    pooled_rate_over25 = mean(speed > 25) * Y,
    pooled_rate_over30 = mean(speed > 30) * Y,
    .groups = "drop"
  ) %>%
  arrange(location_id, period)

loc_labels <- c(ri_ave = "RI Ave", tanner_park = "Tanner Park")

mode_df <- full_df |>
  filter(location_id %in% c("ri_ave", "tanner_park")) |>
  mutate(
    phase = if_else(date < as.Date("2025-11-10"), "Baseline", "Intervention"),
    mode = case_when(
      traveler_type == "pedestrian"       ~ "Pedestrian",
      traveler_type == "bicycle"          ~ "Bike",
      traveler_type == "electric_vehicle" ~ "E-vehicle"),
    phase = factor(phase, levels = c("Baseline", "Intervention")),
    mode  = factor(mode, levels = c("Pedestrian", "Bike", "E-vehicle"))
  ) |>
  count(location_id, phase, mode, name = "n") |>
  group_by(location_id, phase) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

mode_split_plot <- ggplot(mode_df, aes(x = mode, y = pct, fill = phase)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(
    aes(label = percent(pct, accuracy = 0.1)),
    position = position_dodge(width = 0.85),
    vjust = -0.35
  ) +
  facet_wrap(~ location_id, labeller = as_labeller(loc_labels)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1), limits = c(0, NA)) +
  scale_fill_manual(values = c("Baseline" = "#6B6B6B", "Intervention" = "#2C5AA0")) +
  labs(
    title = "Mode split by location: baseline vs intervention",
    y = "Percent of observations"
  ) +
  theme_minimal()

# EXPORTS
save_fig(pct_speeding_plot, "pct_speeding_by_week.png", width = 10, height = 6)
save_fig(tail_speed_plot, "speed_tail_by_week.png", width = 10, height = 6)
save_fig(mode_split_plot, "mode_split_baseline_vs_intervention.png", width = 10, height = 6)

# Helper markdown strings for embedding figures inline in Quarto text
pct_speeding_md <- fig_md(
  "pct_speeding_by_week.png",
  "% speeding (>15 mph) by week relative to intervention"
)

speed_tail_md <- fig_md(
  "speed_tail_by_week.png",
  "Upper-tail speed percentiles by week relative to intervention"
)

mode_split_md <- fig_md(
  "mode_split_baseline_vs_intervention.png",
  "Mode split by location: baseline vs intervention"
)

# Incidence table (per 1,000) as an inline markdown table
incidence_table <- pooled_incidence |>
  select(location_id, period, n_weeks, pooled_rate_over15, pooled_rate_over20, pooled_rate_over25, pooled_rate_over30) |>
  rename(
    Location = location_id,
    Period = period,
    `# Weeks` = n_weeks,
    `over 15 mph (per 1000)` = pooled_rate_over15,
    `over 20 mph (per 1000)` = pooled_rate_over20,
    `over 25 mph (per 1000)` = pooled_rate_over25,
    `over 30 mph (per 1000)` = pooled_rate_over30
  )

incidence_table_md <- knitr::kable(incidence_table)

# convert to markdown tables 
speed_thresholds_md <- knitr::kable(speed_threshold_table)
speed_summary_table_md <- knitr::kable(baseline_speed_summary_table)

# Save key outputs as objects for exporting
baseline_missingness_plot <- missing_data
baseline_speeding_volume_by_period_plot <- plot_weekday_weekend

# EXPORTS
save_fig(baseline_missingness_plot, "baseline_missingness.png", width = 9, height = 6)
save_fig(baseline_speeding_volume_by_period_plot, "baseline_speeding_volume_weekday_weekend.png", width = 11, height = 7)
save_fig(baseline_speeds_by_location_plot, "baseline_speeds_by_location.png", width = 10, height = 6)
save_fig(avg_users_plot, "baseline_avg_users_by_weekday.png", width = 10, height = 6)
save_fig(speed_phase_plot, "speeds_by_location_baseline_vs_intervention.png", width = 12, height = 6)
