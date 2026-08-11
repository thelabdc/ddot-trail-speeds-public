# SETUP CODE: INSTALL PACKAGES AND CREATE EXPORT HELPER FUNCTIONS

# PURPOSE: One-time setup for this repo. Installs required CRAN packages, labtoolbox, and alerts user of missing non-CRAN packages. Sets up standard output folders (docs, figs, tables) and helper functions to 
# save tables, figures, and Quarto documents directly to the repo.  NOTE: NON-CRAN PACKAGES MUST BE INSTALLED MANUALLY BY THE USER. SEE README.MD FOR INSTRUCTIONS.

# PACKAGE INSTALLS
cran_pkgs <- c("tidyverse", "data.table", "randomizr", "here", "knitr") # required CRAN packages
noncran_pkgs <- c("tibblify", "openmeteo", "labtoolbox") # Required non-CRAN packages

# Install CRAN packages
install_from_cran <- function(pkgs) {
  to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(to_install) == 0) {
    message("All CRAN packages already installed.")
    return(invisible(TRUE))
  }
  message("Installing CRAN packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
  invisible(TRUE)
}

install_from_cran(cran_pkgs)

# Install labtoolbox from GitHub
if (!requireNamespace("labtoolbox", quietly = TRUE)) {
  message("\nInstalling labtoolbox from GitHub...")
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("thelabdc/labtoolbox")
}

missing_noncran <- noncran_pkgs[!vapply(noncran_pkgs, requireNamespace, logical(1), quietly = TRUE)] # Report missing packages

if (length(missing_noncran) > 0) {
  message("\nMissing non-CRAN packages: ", paste(missing_noncran, collapse = ", "))
  message("See the README for package installation instructions. Re-run this script when install is complete to ensure all packages were correctly installed and loaded.")
} else {
  message("\nAll required packages appear available.")
}

# DEFINE HELPER FUNCTIONS AND EXPORT DIRECTORY PATHS
if (!requireNamespace("here", quietly = TRUE)) {
  stop("Missing package 'here'. Install with: install.packages('here')")
}

# Export paths
docs_dir <- here::here("docs")
figs_dir <- here::here("figs")

dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figs_dir, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(p, filename, width = 9, height = 6, dpi = 300, bg = "white") {
  if (is.null(p)) return(invisible(NULL))
  ggsave(
    filename = file.path(figs_dir, filename),
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg
  )
}
fig_md <- function(filename, caption = NULL) {
  if (is.null(caption) || is.na(caption) || caption == "") {
    return(paste0("![](../figs/", filename, ")"))
  }
  paste0("![](../figs/", filename, "){fig-cap=\"", caption, "\"}")
}
