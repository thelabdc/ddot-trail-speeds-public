# DDOT-trail-speeds-public
CRediT statement (Roles modified for code/analysis):
- [Collin Brown](https://github.com/clb3122): Data curation (original draft), Data analysis (original draft), Methodology 
- [Alyssa Huberts](https://thelabprojects.dc.gov/alyssa-huberts): Data curation (review and editing), Data analysis (review and editing, original draft), Methodology 
- [Ryan T Moore](https://thelabprojects.dc.gov/ryan-moore): Supervision, Methdology

## Overview

This repository contains the R workflow for the Metropolitan Branch Trail speed feedback sign pilot. It includes scripts to randomize sign messages, clean and prepare speed data, conduct baseline and intervention analyses, estimate event-study models, and generate figures.

The analysis focuses on speed feedback sign data from Rhode Island Ave and Alethia Tanner Park, with additional downstream counter analysis at Tanner Park.

To see the associated slide deck for this project, click [here](https://thelabprojects.dc.gov/trail-speeds).

## Requirements

Core R packages include:

- `broom`
- `data.table`
- `here`
- `hms`
- `knitr`
- `openmeteo`
- `quarto`
- `randomizr`
- `scales`
- `tidyverse`

Running `code/00_setup.R` will install required packages, create output folders, and define helper functions.

## Repository Structure

Scripts are numbered in the order they should be run:

- `00_setup.R` — One-time setup for this repo. Installs required CRAN packages, `{labtoolbox}`, and alerts user of missing non-CRAN packages. Sets up standard output folders (docs, figs, tables) and helper functions to save tables and figures directly to the repo. NON-CRAN PACKAGES MUST BE INSTALLED MANUALLY:
  - This project uses three packages that will need to be installed manually: `{labtoolbox}`, `{openmeteo}`, and `{tibblify}`.
  - Install `{sysfonts}` and `{labtoolbox}` from GitHub:
    ```
    install.packages("pak")
    pak::pak("sysfonts")
    pak::pak("thelabdc/labtoolbox")
    ```
  - Install `{openmeteo}` and `{tibblify}` with:
    ```
      pak::pak("tpisel/openmeteo")
      pak::pak("wranglezone/tibblify")
    ```
- `01_randomize_message.R` — Create a reproducible day-by-day message assignment for each sign.
- `02_sfs_data_cleaning.R` — Load sign radar data, join with message randomziation list, create analysis variables, and add weather data.
- `03_downstream_counter_cleaning.R` — Loads downstream counter data, join with message sign data, create analysis variables, and add weather data.
- `04_descriptive_analyses.R` — Baseline (pre-intervention) diagnostics and descriptive plots.
- `05_event_study.R` — Estimate weekly changes in speed (mph) after the intervention, separately by location using an event study specification with controls.
- `06_downstream_analysis.R` — Uses the counter to replicate the event studies and also run a DID between counter and SFS
- `07_message_testing.R` — Test whether upstream message ("text" vs "image") relates to downstream avg speed (15-min bins).
- `run_all.R` —  Runs all files in repo and reproduces workflow.

## Data

The `data/` directory in the private version of this repository contains speed feedback sign data, message randomization outputs, and downstream counter data.

