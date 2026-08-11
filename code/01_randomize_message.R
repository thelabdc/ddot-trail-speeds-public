# MBT SPEED FEEDBACK SIGN MESSAGE RANDOMIZATION

# PURPOSE: Create a reproducible day-by-day message assignment for each sign.
# INPUTS: Date seeds + pilot date windows
# OUTPUTS: CSV randomization file in data/message randomization/

library(tidyverse)
library(randomizr)

# Randomization Design: We randomized which message was shown each day at each sign. We used a block random assignment using an 8-day block schedule so that message assignment stays balanced and one message is 
# not shown more often than the other. Each day is labeled with a two-sign message combination code: image-image, image-text, text-image, or text-text. The first letter represents the message shown at the Tanner 
# Park sign and the second letter represents the message shown at the RI Ave sign on a given day. For shorthand, we use AA, AB, BA, and BB, where A = image and B = text. For example, “AB” means Tanner Park shows 
# an image and RI Ave shows text that day. Per 8-day block, each message combination happens twice, which makes text and image messages appear roughly the same number of days.

# Randomization helper function
run_message_randomization <- function(seed,
                                      start_date,
                                      end_date,
                                      outfile = NULL,
                                      conditions = c("AA", "AB", "BA", "BB")) {
  set.seed(seed)
  
  pilot_dates <- data.frame(
    date = seq.Date(from = as.Date(start_date),
                    to = as.Date(end_date),
                    by = "day")
  ) |>
    mutate(week = ceiling(row_number() / 8)) |>
    mutate(
      combo_group = block_ra(blocks = week, conditions = conditions)
    ) |>
    mutate(
      tanner_park_sign = case_when(
        substr(combo_group, 1, 1) == "A" ~ "image",
        substr(combo_group, 1, 1) == "B" ~ "text"
      ),
      ri_ave_sign = case_when(
        substr(combo_group, 2, 2) == "A" ~ "image",
        substr(combo_group, 2, 2) == "B" ~ "text"
      )
    ) |>
    select(-combo_group, -week) 
  
  # quick balance check
  print(pilot_dates |>
          group_by(tanner_park_sign, ri_ave_sign) |>
          count())
  
  pilot_dates
}

# Save outputs under data/message randomization/
msg_dir <- file.path("data", "message randomization")
dir.create(msg_dir, recursive = TRUE, showWarnings = FALSE)

# First randomized 10/21/2025 at 4:33pm
pilot_dates_1 <- run_message_randomization(
  seed = 25102116,
  start_date = "2025-11-10",
  end_date = "2025-12-21"
)

# Second randomization 12/19/2025 at 11:05am
pilot_dates_2 <- run_message_randomization(
  seed = 25121911,
  start_date = "2025-12-22",
  end_date = "2026-03-01"
)

# Full file for analysis
pilot_dates <- bind_rows(pilot_dates_1, pilot_dates_2) #

# Export randomization files
write.csv(pilot_dates_1, file.path(msg_dir, "MBT_sign_message_randomization_1.csv"), row.names = FALSE) # Pilot dates 11/10/2025 - 12/21/2025
write.csv(pilot_dates_2, file.path(msg_dir, "MBT_sign_message_randomization_2.csv"), row.names = FALSE) # Pilot dates 12/22/2025 - 03/01/2026
write.csv(pilot_dates, file.path(msg_dir, "MBT_sign_message_randomization_full_pilot.csv"), row.names = FALSE) # Full pilot 10/23/2025 - 03/01/2026