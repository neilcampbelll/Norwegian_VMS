if (!require("pacman")) install.packages("pacman")

# Install vmstools from GitHub if not already installed
if (!require("vmstools", quietly = TRUE)) {
  pacman::p_load(devtools)
  install_github("nielshintzen/vmstools/vmstools/")
}

pacman::p_load(
  dplyr,
  lubridate,
  vmstools,
  icesVocab,
  httr,
  stringr,
  tidyr,
  rvest,
  sf,
  tidyverse,
  lubridate,
  readr
)

data("ICESareas")