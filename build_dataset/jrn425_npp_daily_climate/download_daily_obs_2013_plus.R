### Downloading online data for daily precipitation and temperature from 15 NPP sites

### Data from wireless sensors can be downloaded from EDI

library(tidyverse)

source("daily_climate/scripts/download_utils.R")

#Download all site data
daily_climate <- getAllSites()

# Save a local copy of downloaded data
daily_climate %>%
  write.csv("daily_climate/raw/daily_climate_2013_present.csv",
            row.names = FALSE,
            quote = FALSE)
  