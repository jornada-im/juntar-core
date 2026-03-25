### This file reads in the monthly ppt data and then calculates multiple annual summaries:
###    Annual: The sum of months in the same calendar year
###    WY: The sum of months in the same water year (Oct-Sept)
###    GS: The sum of Apr-Aug, the growing season (seasonal value, but assigned to each year, so considered 'annual' summary)
## Heather Savoy (hsavoy.jrn.lter@gmail.com)
## Started 02/27/2019

library(tidyverse)

## the working directory is assumed to be the NPP-data repo

## 1. Read in the monthly summary file
monthly_sum <- read_csv("ppt/processed/PPT_monthly.csv") 

## 2. Create each annual summary by site
# Annual
annual <- monthly_sum %>%
  group_by(site,year) %>%
  summarise(p_cm = sum(p_cm, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(type = "annual_cm")

# Water year
WY <- monthly_sum %>%
  mutate(WY = ifelse(month < 10, year, year+1)) %>%  #assign water year
  group_by(site,WY) %>%
  summarise(p_cm = sum(p_cm, na.rm = TRUE)) %>%
  rename(year = WY) %>%
  ungroup %>%
  mutate(type = "wy_cm")

# Growing season
GS <- monthly_sum %>%
  filter(month %in% 4:9) %>% # filter to growing season
  group_by(site,year) %>%
  summarise(p_cm = sum(p_cm, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(type = "gs_cm")


## 3. Join each annual summary by site and year
annual_sums <- annual %>%
  bind_rows(WY) %>%
  bind_rows(GS) %>%
  spread(type,p_cm)

## 4. Store the annual summaries as a .csv in the 'processed' folder. 
write.csv(annual_sums,
          file = "ppt/processed/PPT_annuals.csv",
          quote = FALSE,
          row.names = FALSE)

