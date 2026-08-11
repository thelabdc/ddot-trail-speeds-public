# PURPOSE: Test whether upstream message ("text" vs "image") relates to downstream avg speed (15-min bins).
# INPUTS: data/NCR_downstream.csv + full_df.csv.
# OUTPUTS: average speed difference by message and Quarto document.

library(tidyverse)
library(lubridate)
library(broom)
library(labtoolbox)
library(hms)

intervention_start <- as.Date("2025-11-10")

#Downstream counter: 15-min avg speed (intervention, 07:00–19:00) - uses same data as DiD cleaning script
counter_inbound <- did_long |>
  filter(source == "Tanner_Counter", post == 1) |>
  transmute(
    bin_15min,
    n_obs_counter = n_obs_total,
    avg_speed
  ) |>
  filter(n_obs_counter > 0) |>
  filter(
    as_hms(bin_15min) >= as_hms("07:00:00"),
    as_hms(bin_15min) <= as_hms("19:00:00")
  )

# Upstream message: Tanner Park 15-min bins (intervention, speed>=8)
msg_by_bin <- full_df |>
  filter(location_id == "tanner_park") |>
  mutate(
    timestamp = ymd_hms(timestamp, tz = "America/New_York"),
    bin_15min = floor_date(timestamp, "15 minutes"),
    date = as.Date(bin_15min),
    message = str_to_lower(as.character(message))
  ) |>
  filter(date >= intervention_start, speed >= 8) |>
  group_by(bin_15min) |>
  summarise(
    message = if_else(n_distinct(message) == 1, first(message), NA_character_),
    .groups = "drop"
  ) |>
  drop_na(message) |>
  mutate(message = relevel(factor(message), ref = "text"))

# Join + perform regression
downstream_msg_df <- inner_join(counter_inbound, msg_by_bin, by = "bin_15min") |>
left_join(long_data_counter[,c("bin_15min", "hour_of_week", "temp_f", "precip_mm", "wind_mph_10", "light_factor", "wday", "week", "weeks_rel_intervention_factor", "vol_15_min", "ped_share_15_min", "hour")], by = "bin_15min")
m_avg <- lm(avg_speed ~ message, data = downstream_msg_df)

msg_row <- tidy(m_avg) |> filter(term == "messageimage")
diff_avg <- msg_row$estimate
pval_avg <- msg_row$p.value

# Plot
p_avg <- make_ci_barplot(
  model = m_avg,
  treat_arms = "messageimage",
  ci_level = 0.95,
  plot_caption = NULL
) +
  ggtitle("Downstream avg speed: Image vs Text") +
  labs(subtitle = "", x = NULL, y = "Estimated avg speed (mph)") +
  scale_x_discrete(labels = c(Control = "Text", messageimage = "Image")) +
  scale_fill_manual(values = c(Control = "grey40", messageimage = "steelblue")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5)) +
  annotate(
    "text", x = 1.5, y = Inf,
    label = paste0(
      "Image - Text = ", sprintf("%.2f", diff_avg),
      " mph (p=", format.pval(pval_avg, digits = 2, eps = 1e-3), ")"
    ),
    vjust = 1.3, fontface = "bold", size = 3.2
  ) +
  coord_cartesian(clip = "off")

save_fig(p_avg, "downstream_avg_speed_image_vs_text.png", width = 9, height = 6)

# Message with controls: 
m_avg_with_controls <- lm(avg_speed ~ message + wday + hour + temp_f +
                            precip_mm + wind_mph_10 + vol_15_min + ped_share_15_min + light_factor, data = downstream_msg_df)
