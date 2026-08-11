# PURPOSE: Runs all files in repo and reproduces workflow.

source("code/00_setup.R")
source("code/01_randomize_message.R")
source("code/02_sfs_data_cleaning.R")
source("code/03_downstream_counter_cleaning.R")
source("code/04_descriptive_analyses.R")
source("code/05_event_study.R")
source("code/06_downstream_analysis.R")
source("code/07_message_testing.R")
message("Run all complete.")