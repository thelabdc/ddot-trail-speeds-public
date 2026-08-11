# Analyses related to downstream (NCR counter) data
# PURPOSE: Leverages the counter to replicate the event studies and also run a DID between counter and sfs
# INPUT: sign data and downstream counter data
# OUTPUT: DiD downstream speed effect model and estimate; Event study plots 

# DID: Did seeing the sign in the intervention period make people slow down between the sign and the counter? 
# The true DID specification below is the most robust way to do this. We also make a plot to illustrate the logic. 

did_speed <- lm(
  avg_speed ~ source + post + post * source + hour_of_week + precip_mm + wind_mph_10 + temp_f + light_factor + vol_15_min + ped_share_15_min,
  data = did_long
)

downstream_did_coef <- broom::tidy(did_speed) |>
  filter(term == "sourceTanner_Counter:post") # This is the DiD effect 

# DID on egregious speed 
did_es <- lm(
  pct_above_22mph ~ source + post + post * source + hour_of_week + precip_mm + wind_mph_10 + temp_f + light_factor + vol_15_min + ped_share_15_min,
  data = did_long
)


# Downstream graph to visualize the logic of the DID (for interpretability) 
# Speed 
did_summary_speed <- did_long |>
  mutate(period = case_when(
    isoweek(bin_15min) == 44 ~ "pre_1",
    isoweek(bin_15min) == 45 ~ "pre_2",
    TRUE ~ "post"
  )) |>
  group_by(source, period) |>
  summarize(average_speed = mean(avg_speed, na.rm = TRUE), .groups = "drop") |>
  mutate(period = factor(period,
                         levels = c("pre_1","pre_2","post"),
                         labels = c("Week 1",
                                    "Week 2",
                                    " ")))|>
  mutate(source = factor(source, levels = c("Tanner_SFS", "Tanner_Counter"), 
                         labels = c("Speed Feedback Sign", "Downstream Counter")))

period_avgs_speed <- did_long |>
  mutate(source = factor(source, levels = c("Tanner_SFS", "Tanner_Counter"), 
                         labels = c("Speed Feedback Sign", "Downstream Counter"))) |>
  group_by(source, post) |>
  summarize(period_avg = mean(avg_speed, na.rm = TRUE), .groups = "drop")

# The x axis (after factor labels) has 3 positions: 1, 2, 3.
# The midpoint between pre_1 and pre_2 is x = 1.5.
label_positions_speed <- period_avgs_speed |>
  mutate(
    x = ifelse(post == 0, 1.5, 3.25),  # midpoint on the x scale
    y = case_when(post == 0 ~ period_avg - .1, 
                  post == 1 & source == "Speed Feedback Sign"~ period_avg - .05,
                  post == 1 & source == "Downstream Counter"~ period_avg + .05),
    label = ifelse(post == 0, 
                   paste0("Baseline:\n(2-week average)\n", round(period_avg, 1), " mph"),
                   paste0("Intervention:\n(8-week average)\n", round(period_avg, 1), " mph")
    )
  ) 

(ggplot(did_summary_speed) +
  geom_point(aes(x = period, y = average_speed, color = source)) +
  geom_line(aes(x = period, y = average_speed, color = source, group = source)) +
  geom_text(
    data = label_positions_speed,
    aes(x = x, y = y, label = label, color = source),
    size = 12, lineheight = 0.25
  ) +
    annotate("rect",
             xmin = 0.5, xmax = 2.5,
             ymin = -Inf, ymax = Inf,
             alpha = 0.1, fill = "grey90") +
    annotate("rect",
             xmin = 2.5, xmax = Inf,
             ymin = -Inf, ymax = Inf,
             alpha = 0.1, fill = "grey70") +
    annotate("text",
             x = 1.5, y = 14.8,
             label = "Baseline",
              size = 15)+ 
    annotate("text",
             x = 3, y = 14.8,
             label = "Intervention",
             size = 15)+ 
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "Change in Average Speeds",
    x = "",
    y = "Average Speed (mph)",
    color = ""
  ) +
  #coord_cartesian(ylim = c(14.5, 16)) +
  theme_lab(ppt = TRUE) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_blank())
)|>
  ggsave(file = "figs/did_plot_avg_speed.png", width = 8, height = 5)

# Egregious speeding 
did_summary_eg <- did_long |>
  mutate(period = case_when(
    isoweek(bin_15min) == 44 ~ "pre_1",
    isoweek(bin_15min) == 45 ~ "pre_2",
    TRUE ~ "post"
  )) |>
  group_by(source, period) |>
  summarize(pct_above_22mph = mean(pct_above_22mph, na.rm = TRUE), .groups = "drop") |>
  mutate(period = factor(period,
                         levels = c("pre_1","pre_2","post"),
                         labels = c("Week 1",
                                    "Week 2",
                                    " ")))|>
  mutate(source = factor(source, levels = c("Tanner_SFS", "Tanner_Counter"), 
                         labels = c("Speed Feedback Sign", "Downstream Counter")))

period_avgs_eg <- did_long |>
  mutate(source = factor(source, levels = c("Tanner_SFS", "Tanner_Counter"), 
                         labels = c("Speed Feedback Sign", "Downstream Counter"))) |>
  group_by(source, post) |>
  summarize(period_avg = mean(pct_above_22mph, na.rm = TRUE), .groups = "drop")

# The x axis (after factor labels) has 3 positions: 1, 2, 3.
# The midpoint between pre_1 and pre_2 is x = 1.5.
label_positions_eg <- period_avgs_eg |>
  mutate(
    x = ifelse(post == 0, 1.5, 3.25),  # midpoint on the x scale
    #y = case_when(post == 0 ~ period_avg - .1, 
    #              post == 1 & source == "Speed Feedback Sign"~ period_avg - .05,
    #              post == 1 & source == "Downstream Counter"~ period_avg + .05),
    y = period_avg + .01,
    label = ifelse(post == 0, 
                   paste0("Baseline:\n(2-week average)\n", 100*round(period_avg, 3), "%"),
                   paste0("Intervention:\n(8-week average)\n", 100*round(period_avg, 3), "%")
    )
  ) 

(ggplot(did_summary_eg) +
    geom_point(aes(x = period, y = pct_above_22mph, color = source)) +
    geom_line(aes(x = period, y = pct_above_22mph, color = source, group = source)) +
    geom_text(
      data = label_positions_eg,
      aes(x = x, y = y, label = label, color = source),
      size = 12, lineheight = 0.25
    ) +
    annotate("rect",
             xmin = 0.5, xmax = 2.5,
             ymin = -Inf, ymax = Inf,
             alpha = 0.1, fill = "grey90") +
    annotate("rect",
             xmin = 2.5, xmax = Inf,
             ymin = -Inf, ymax = Inf,
             alpha = 0.1, fill = "grey70") +
    annotate("text",
             x = 1.5, y = .02,
             label = "Baseline",
             size = 15)+ 
    annotate("text",
             x = 3, y = .02,
             label = "Intervention",
             size = 15)+ 
    scale_y_continuous(labels = scales::label_percent())+
    scale_x_discrete(drop = FALSE) +
    labs(
      title = "Change in Egregious Speeding",
      x = "",
      y = "Percent Egregious Speeding",
      caption = str_wrap("For the downstream analyses, we define egregious speeding as 22+ mph based on the counter data structure.",60),
      color = ""
    ) +
    scale_color_discrete(palette = c("#17375E", "#C00000")) + 
    coord_cartesian(ylim = c(.0, .12)) +
    theme_classic(base_family = "Neutra")  + 
    theme(plot.title = element_text(size = 60,  hjust = 0.5, lineheight = 0.4),
          plot.subtitle = element_text(size = 55), 
          plot.caption = element_text(size = 40, hjust = 1,  lineheight = 0.4),
          axis.title = element_text(size = 40),
          legend.title = element_text(size = 45, lineheight = 0.25),
          legend.text = element_text(size = 45)  
          ) +  
    theme(plot.caption = element_text(size = 30, color = "grey30", lineheight = .25)) + 
    theme(legend.position =  "top")
  )|>
  ggsave(file = "figs/did_plot_eg.png", width = 8, height = 5.25)


# Event Studies
# Event Study at the Counter (Average speed)
es_model_counter <- lm(avg_speed ~ weeks_rel_intervention_factor + light_factor + temp_f + precip_mm + wind_mph_10 + factor(wday) + hour   + vol_15_min + ped_share_15_min, data = long_data_counter)

es_coefs_counter <- es_model_counter |>
  tidy(conf.int = TRUE)|>
  filter(str_detect(term, "weeks_rel_intervention_factor")) |>
  rename(weeks_rel_intervention= term)|>
  mutate(weeks_rel_intervention = as.double(str_replace_all(string=weeks_rel_intervention, pattern = "weeks_rel_intervention_factor",replace= "")))

week_window <- c(-2, -1, 1:8)

es_coefs_counter <- es_coefs_counter |>
  tidyr::complete( weeks_rel_intervention = week_window) |>
  mutate(
    estimate  = if_else(weeks_rel_intervention == -1, 0, estimate),
    conf.low  = if_else(weeks_rel_intervention ==-1, NA_real_, conf.low),
    conf.high = if_else(weeks_rel_intervention == -1, NA_real_, conf.high)
  )

es_plot_counter <-  ggplot(es_coefs_counter) + 
    geom_point(aes(x = weeks_rel_intervention, y = estimate)) + 
    geom_errorbar(aes(x = weeks_rel_intervention, ymin =conf.low, ymax = conf.high
                      ),width = .2, color = "grey25")+ 
    theme_classic() + 
    geom_vline(aes(xintercept = 0), color = "blue", linetype = "dashed")+
    geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
    labs(x = "Weeks Relative to Intervention", y = "Estimated Effect") + 
  labs(title = "Average Speeds Measured at Tanner Park \n Eco Visio Counter \n (Event Study)")+
  theme(plot.title = element_text(size = 55, lineheight = .4),
        axis.title = element_text(size = 45), 
        axis.text = element_text(size = 40)) + 
  ylim(-10,10)

save_fig(es_plot_counter, "event_study_tanner_counter.png", width = 9, height = 6)

# Event Study at the Counter (% over 22mph) (Egregious speeding)
# Note that this is what we have bins for- otherwise we're imposing a cutoff on an average taken from cutoffs, which feels odd
es_model_egregious_speed_counter <- lm(pct_above_22mph ~ weeks_rel_intervention_factor + light_factor + temp_f + precip_mm + wind_mph_10 + factor(wday)   + vol_15_min + ped_share_15_min, data = long_data_counter)

es_coefs_egregious_speed_counter <- es_model_egregious_speed_counter |>
  tidy(conf.int = TRUE)|>
  filter(str_detect(term, "weeks_rel_intervention_factor")) |>
  rename(weeks_rel_intervention= term)|>
  mutate(weeks_rel_intervention = as.double(str_replace_all(string=weeks_rel_intervention, pattern = "weeks_rel_intervention_factor",replace= "")))

es_coefs_egregious_speed_counter <- es_coefs_egregious_speed_counter |>
  tidyr::complete( weeks_rel_intervention = week_window) |>
  mutate(
    estimate  = if_else(weeks_rel_intervention == -1, 0, estimate),
    conf.low  = if_else(weeks_rel_intervention ==-1, NA_real_, conf.low),
    conf.high = if_else(weeks_rel_intervention == -1, NA_real_, conf.high)
  )

es_plot_egregious_speed_counter <-  ggplot(es_coefs_egregious_speed_counter) + 
  geom_point(aes(x = weeks_rel_intervention, y = estimate)) + 
  geom_errorbar(aes(x = weeks_rel_intervention, ymin =conf.low, ymax = conf.high  ), width = .2, color = "grey25")+ 
  theme_classic() + 
  geom_vline(aes(xintercept = 0), color = "blue", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  labs(x = "Weeks Relative to Intervention", y = "Estimated Effect") + 
  labs(title = "Percent Traveling at Speeds above 22mph at Tanner Park \n Eco Visio Counter \n (Event Study)")+
  theme(plot.title = element_text(size = 55, lineheight = .4),
      axis.title = element_text(size = 45), 
      axis.text = element_text(size = 40)) + 
  ylim(-.25, .25)

save_fig(es_plot_egregious_speed_counter, "event_study_tanner_counter_es.png", width = 9, height = 6)

# What can we say about egregious speeding at the counter? 
mean(long_data_counter$pct_above_22mph, na.rm = TRUE) # About 4.5% (slightly higher than at the SFS)
long_data_counter |>
  filter(weeks_rel_intervention_factor %in% c("-1", "-2"))|>
  summarize(mean(pct_above_22mph, na.rm = TRUE)) # About 5.2%

long_data_counter |>
  filter(weeks_rel_intervention_factor %in% c("1", "2", "3", "4", "5", "6", "7", "8"))|>
  summarize(mean(pct_above_22mph, na.rm = TRUE)) # About 4.2%
