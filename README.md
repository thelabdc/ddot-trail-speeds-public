# DDOT-trail-speeds
@authors
CRediT statement (Roles modified for code/analysis):
- (Collin Brown)[https://thelabprojects.dc.gov/collin-brown]: Data curation (original draft), Data analysis (original draft), Methodology 
- (Alyssa Huberts)[https://thelabprojects.dc.gov/alyssa-huberts]: Data curation (review and editing), Data analysis (review and editing, original draft), Methodology 
- (Ryan T Moore)[https://thelabprojects.dc.gov/ryan-moore]: Supervision, methdology

## Introduction

This repo contains R scripts for randomizing speed feedback sign messages posted on the Metropolitan Branch Trail, as well as scripts used to analyze pilot data.

#### Requirements:

The primary packages (or groups of packages) used here for R are:
- `tidyverse` for data cleaning, manipulation, and modeling
- `randomizr` for the sign message block randomization
- `openmeteo` (requires manual installation; depreciated by CRAN)
- `tidymodels` for the event study code

The relevant packages and modules are included at the top of each R script.

## Organization

The repo is organized in three primary directories:
- `code` includes scripts needed to run the message randomization
- `docs` 
- `figs` 

Additionally, this repository includes `README`, `.gitignore`, and `.Rproj` 
files.

## `/code/`

This directory includes these files:

- `00_package-installation.R` installs packages and dependencies manually that were depreciated by CRAN but are necessary to run analysis code.
- `01_randomize-message.R` replicates block randomization code, including timestamped seeds to denote when the code was run. This code was run twice to account for an extension of the pilot.
- `02_data_cleaning.R` loads in data files, combines them into one main file, manipulates the variables to be useful for analysis, adds weather data, computes volume, and joins message assignment to each observation.
- `03_baseline_data_analyses.R` performs analyses on the baseline data period (October 23-November 9) to explore what data is missing, as well as user volume and speed trends at each location prior to the start of the intervention.
- `04_descriptive_analyses.R` displays all descriptive tables and plots.
- `05_event_study.R` includes main event study code for primary pilot analyses.
- `06_exploratory_analyses.R` 

## `/data/`

This directory contains all data pulls supplied by SaferStreet Solutions. "Gold Dragon" denotes Alethia Tanner Park sign data, while "Gold Goose" denotes Rhode Island Ave sign data. 
File naming convention is as follows: location-id_through_YYYY-MM-DD_HHMM.csv
