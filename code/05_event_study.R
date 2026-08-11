# PURPOSE: Estimate weekly changes in speed (mph) after the intervention, separately by location using an event study specification with controls.
# INPUTS: over_7mph, subsettted from full_df, and desired control variables.
# OUTPUTS: weekly coefficient estimates with 95% CIs and event study plots by location

library(tidyverse)
library(broom)

# speeds over 7 mph only
over_7mph <- full_df |> 
  filter(speed >= 7) 

# speeds over 7 mph only, between 8-9:30 am morning rush
over_7mph_AM <- full_df |> 
  filter(speed >= 7) |>
  filter((hour == 8) | (hour == 9 & minute(timestamp) < 30))

# Week date labels by location

week_window <- c(-2, -1, 1:8)

week_date_map <- full_df |>
  group_by(location_id, week_rel_factor) |>
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date   = max(date, na.rm = TRUE),
    n = n(),
    .groups = "drop") |>
  arrange(location_id, as.numeric(as.character(week_rel_factor))) |>
  mutate(
    week = as.integer(as.character(week_rel_factor)),
    label = paste0(format(start_date, "%m/%d"), "–", format(end_date, "%m/%d"))) |>
  filter(week %in% week_window)

# One object per location with breaks and labels for ggplot
ria_week_labels <- week_date_map |>
  filter(location_id == "ri_ave") |>
  arrange(week)
x_breaks_ria <- ria_week_labels$week
x_labels_ria <- ria_week_labels$label

atp_week_labels <- week_date_map |>
  filter(location_id == "tanner_park") |>
  arrange(week)
x_breaks_atp <- atp_week_labels$week
x_labels_atp <- atp_week_labels$label

# Fit a model per location + extract the week coefficients. Each week_rel_factor coefficient is the estimated change in speed (mph) relative to week -1, holding controls constant.

coef_tbl <- over_7mph |>
  mutate(
    week_rel_intervention = as.integer(week_rel_intervention),
    week_rel_factor = droplevels(week_rel_factor)
  ) |>
  filter(week_rel_intervention %in% week_window) |>
  group_by(location_id) |>
  nest() |>
  mutate(
    tidied = map(data, ~ tidy(lm(speed ~ week_rel_factor + weekday + time + temp_f +
                                   precip_mm + wind_mph_10 + volume_5mins + pct_ped_5mins + light_factor,
                                 data = .x), conf.int = TRUE))) |>
  select(location_id, tidied) |>
  unnest(tidied) |>
  filter(str_detect(term, "^week_rel_factor")) |>
  mutate(week = as.integer(str_extract(term, "-?\\d+")))

# Add an explicit dot at the reference week (-1) for plotting only
# (This week is the model's reference category, so it is defined to be 0 and has no CI.)
coef_tbl <- coef_tbl |>
  ungroup() |>
  tidyr::complete(location_id, week = week_window) |>
  mutate(
    estimate  = if_else(week == -1, 0, estimate),
    conf.low  = if_else(week == -1, NA_real_, conf.low),
    conf.high = if_else(week == -1, NA_real_, conf.high)
  )

#  Plot helper + plots 
y_lim <- range(c(coef_tbl$conf.low, coef_tbl$conf.high, 0), na.rm = TRUE)

make_plot <- function(d, title_txt, x_breaks, x_labels) {
  ggplot(d, aes(x = week, y = estimate)) +
    annotate(
      "label", x = -1.5, y = y_lim[2],
      label = "Baseline", vjust = 1.2, size = 3.5,
      fill = "white", alpha = 0.9
    ) +
    annotate(
      "label", x = 4.0, y = y_lim[2],
      label = "Intervention", vjust = 1.2, size = 3.5,
      fill = "white", alpha = 0.9
    ) +
    geom_hline(yintercept = 0, linetype = 2, color = "gray40") +
    geom_vline(xintercept = 0, color = "#1F77B4", linewidth = 1) +
    geom_errorbar(
      data = d |> filter(!is.na(conf.low), !is.na(conf.high)),
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.2, color = "gray25"
    ) +
    geom_point() +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      limits = c(min(x_breaks, na.rm = TRUE) - 0.3, max(x_breaks, na.rm = TRUE) + 0.3)
    ) +
    coord_cartesian(ylim = y_lim) +
    labs(
      title = title_txt,
      x = "Week",
      y = "Estimated change in speed (mph)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p_ria <- coef_tbl |>
  filter(location_id == "ri_ave") |>
  make_plot("Weekly effects (≥7 mph): RI Ave", x_breaks = x_breaks_ria, x_labels = x_labels_ria)

p_atp <- coef_tbl |>
  filter(location_id == "tanner_park") |>
  make_plot("Weekly effects (≥7 mph): Tanner Park", x_breaks = x_breaks_atp, x_labels = x_labels_atp)

# EXPORTS
save_fig(p_ria, "event_study_ri_ave.png", width = 9, height = 6)
save_fig(p_atp, "event_study_tanner_park.png", width = 9, height = 6)
# Morning commute event study (8:00–9:30am)
coef_tbl_am <- over_7mph_AM |>
  mutate(
    week_rel_intervention = as.integer(week_rel_intervention),
    week_rel_factor = droplevels(week_rel_factor)
  ) |>
  filter(week_rel_intervention %in% week_window) |>
  group_by(location_id) |>
  nest() |>
  mutate(
    tidied = map(data, ~ tidy(lm(speed ~ week_rel_factor + weekday + time + temp_f +
                                   precip_mm + wind_mph_10 + volume_5mins + pct_ped_5mins + light_factor,
                                 data = .x), conf.int = TRUE))) |>
  select(location_id, tidied) |>
  unnest(tidied) |>
  filter(str_detect(term, "^week_rel_factor")) |>
  mutate(week = as.integer(str_extract(term, "-?\\d+")))

# Add an explicit dot at the reference week (-1) for plotting only
coef_tbl_am <- coef_tbl_am |>
  ungroup() |>
  tidyr::complete(location_id, week = week_window) |>
  mutate(
    estimate  = if_else(week == -1, 0, estimate),
    conf.low  = if_else(week == -1, NA_real_, conf.low),
    conf.high = if_else(week == -1, NA_real_, conf.high)
  )

y_lim_am <- range(c(coef_tbl_am$conf.low, coef_tbl_am$conf.high, 0), na.rm = TRUE)
make_plot_am <- function(d, title_txt, x_breaks, x_labels) {
  ggplot(d, aes(x = week, y = estimate)) +
    annotate(
      "label", x = -1.5, y = y_lim_am[2],
      label = "Baseline", vjust = 1.2, size = 3.5,
      fill = "white", alpha = 0.9
    ) +
    annotate(
      "label", x = 4.0, y = y_lim_am[2],
      label = "Intervention", vjust = 1.2, size = 3.5,
      fill = "white", alpha = 0.9
    ) +
    geom_hline(yintercept = 0, linetype = 2, color = "gray40") +
    geom_vline(xintercept = 0, color = "#1F77B4", linewidth = 1) +
    geom_errorbar(
      data = d |> filter(!is.na(conf.low), !is.na(conf.high)),
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.2, color = "gray25"
    ) +
    geom_point() +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      limits = c(min(x_breaks, na.rm = TRUE) - 0.3, max(x_breaks, na.rm = TRUE) + 0.3)
    ) +
    coord_cartesian(ylim = y_lim_am) +
    labs(
      title = title_txt,
      x = "Week",
      y = "Estimated change in speed (mph)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
p_ria_am <- coef_tbl_am |>
  filter(location_id == "ri_ave") |>
  make_plot_am("Weekly effects (≥7 mph, AM): RI Ave", x_breaks = x_breaks_ria, x_labels = x_labels_ria)
p_atp_am <- coef_tbl_am |>
  filter(location_id == "tanner_park") |>
  make_plot_am("Weekly effects (≥7 mph, AM): Tanner Park", x_breaks = x_breaks_atp, x_labels = x_labels_atp)

save_fig(p_ria_am, "event_study_ri_ave_am.png", width = 9, height = 6)
save_fig(p_atp_am, "event_study_tanner_park_am.png", width = 9, height = 6)

write_csv(coef_tbl, file.path(figs_dir, "event_study_week_coefficients.csv"))


### Egregious speeding event study 
# Fit a model per location + extract the week coefficients. Each week_rel_factor coefficient is the estimated change in speed (mph) relative to week -1, holding controls constant.
coef_tbl_es <- over_7mph |>
  mutate(
    week_rel_intervention = as.integer(week_rel_intervention),
    week_rel_factor = droplevels(week_rel_factor)
  ) |>
  filter(week_rel_intervention %in% week_window) |>
  group_by(location_id) |>
  nest() |>
  mutate(
    tidied = map(data, ~ tidy(lm(over_20 ~ week_rel_factor + weekday + time + temp_f +
                                   precip_mm + wind_mph_10 + volume_5mins + pct_ped_5mins + light_factor,
                                 data = .x), conf.int = TRUE))) |>
  select(location_id, tidied) |>
  unnest(tidied) |>
  filter(str_detect(term, "^week_rel_factor")) |>
  mutate(week = as.integer(str_extract(term, "-?\\d+")))

# Add an explicit dot at the reference week (-1) for plotting only
# (This week is the model's reference category, so it is defined to be 0 and has no CI.)
coef_tbl_es <- coef_tbl_es |>
  ungroup() |>
  tidyr::complete(location_id, week = week_window) |>
  mutate(
    estimate  = if_else(week == -1, 0, estimate),
    conf.low  = if_else(week == -1, NA_real_, conf.low),
    conf.high = if_else(week == -1, NA_real_, conf.high)
  )
p_ria_es <- coef_tbl_es |>
  filter(location_id == "ri_ave") |>
  make_plot("Weekly effects on egregious speeding: RI Ave", x_breaks = x_breaks_ria, x_labels = x_labels_ria) + 
  coord_cartesian(ylim = c(-.25, .25)) + 
  theme(plot.title = element_text(size = 55),
        axis.title = element_text(size = 45), 
        axis.text = element_text(size = 40))

p_atp_es <- coef_tbl_es |>
  filter(location_id == "tanner_park") |>
  make_plot("Weekly effects on egregious speeding: Tanner Park Speed Feedback Sign", x_breaks = x_breaks_atp, x_labels = x_labels_atp) + 
  coord_cartesian(ylim = c(-.25, .25)) + 
  theme(plot.title = element_text(size = 55),
        axis.title = element_text(size = 45), 
        axis.text = element_text(size = 40))

save_fig(p_atp_es, "event_study_tanner_park_es.png", width = 9, height = 6)
save_fig(p_ria_es, "event_study_ria_es.png", width = 9, height = 6)
