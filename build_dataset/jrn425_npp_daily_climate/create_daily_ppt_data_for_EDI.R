### Generate this datset on EDI (knb-lter-jrn.210425001)
### for every calendar day 1980 - 2019
### Using three inputs:
###   1. Jin's previously published 1980-2010 dataset
###   2. Jin's 2011- mid 2013 update
###   3. My 2013-2019+ estimates recently performed

library(tidyverse)
library(lubridate)
library(readxl)

### 1. Jin's previously published 1980-2010 dataset
old_files <- list.files("daily_climate/raw/daily_climate_15NPPsites_1980_2010",
                        full.names = TRUE)
p_t_1980_2010 <- tibble()
for(f in old_files){ 
  p_t_1980_2010 <- p_t_1980_2010 %>% 
    bind_rows(read_excel(f))
}
est_1980_2010 <- p_t_1980_2010 %>%
  mutate(date = as.Date(paste(year,month,day,sep="-"))) %>%
  rename(est_daily = PPT_cm) %>%
  dplyr::select(date,site,est_daily)

###   2. Jin's 2011- mid 2013 update
est_2011_2013 <- read_excel("daily_climate/raw/estimated_daily_ppt_jun11.xlsx") %>%
  mutate(date = as.Date(date),
         est_daily = e_daily_mm/10) %>%
  dplyr::select(date,site,est_daily)

# Combine Jin's two datasets
est_1980_2013 <- est_1980_2010 %>%
  bind_rows(est_2011_2013)

# Determine the last date per site in Jin's 2013 estimates
switchSet1 <- est_1980_2013 %>% 
  group_by(site) %>% 
  filter(date == max(date)) %>%
  dplyr::select(-est_daily) %>%
  rename(cutoff = date)

###   3. The CORRECTED mostly-wireless tbrg data
tbrg_2013_2019 <- read_csv("daily_climate/processed/corrected_daily_tbrg.csv") %>%
  complete(site,
           date = full_seq(date,1),
           fill = list(ppt_cm = NA))

# Read in the first date of wireless reporting
switchSet2 <- read_csv("daily_climate/wireless_reporting_start_dates.csv") %>% 
  mutate(date = mdy(date)) %>%
  rename(cutoff = date)

# Find gaps in tbrg after the switch
gaps <- tbrg_2013_2019 %>%
  left_join(switchSet2, by = "site") %>%
  filter(date > cutoff) %>%
  filter(is.na(ppt_cm)) %>%
  dplyr::select(site,date) %>%
  mutate(gap = TRUE)

###   4. My 2013-2018+ estimates recently performed
est_2013_2019 <- read_csv("daily_climate/processed/est_daily_ppt_2013_2019.csv") %>%
  mutate(year = year(date)) %>%
  filter(year %in% 2013:2019) %>% 
  dplyr::select(date,site,est_daily,rowid)

# Find site/dates within grg collection periods with recent tbrg gaps
gap_periods <- est_2013_2019 %>%
  full_join(gaps) %>%
  group_by(site,rowid) %>%
  filter(sum(gap,na.rm = TRUE) > 0) %>% ## the collection periods with a gap
  distinct(site,date,rowid)

# Extract estimated values for those gap periods
est_gaps <- est_2013_2019 %>%
  inner_join(gap_periods) %>%
  dplyr::select(date,site,est_daily)

# Extract estimated values to fill in between Jin's estimated values and the wireless reporting
# only include dates:
#     after Jin's data end and 
#     before wireless reporting began 
est_btw <- est_2013_2019 %>%
  left_join(switchSet1, by = "site") %>%
  left_join(switchSet2, by = "site") %>%
  filter(date > cutoff.x & date < cutoff.y) %>% 
  dplyr::select(date,site,est_daily)

# 5. Filter wireless dataset to after switch
wireless_2013_2019 <- tbrg_2013_2019 %>%
  left_join(switchSet2, by = "site") %>%
  filter(date >= cutoff) %>%
  anti_join(gap_periods) %>%
  filter(!is.na(ppt_cm)) %>%
  rename(est_daily = ppt_cm) %>%
  dplyr::select(date,site,est_daily)

# Combine my and Jin's datasets
est_1980_2019 <- est_1980_2013 %>%
  bind_rows(est_btw) %>%
  mutate(flag = "g") %>%
  bind_rows(wireless_2013_2019) %>%
  bind_rows(est_gaps %>%
              mutate(flag = "g")) %>%
  complete(site,  # confirm all dates present for all sites
           date = full_seq(as.Date(c("1980-01-01","2019-12-31")),1)) %>%
  arrange(date,site)

# Any missing data?
est_1980_2019 %>%
  dplyr::select(-flag) %>%
  filter(!complete.cases(.)) 

# Write whole dataset to file
siteLookup <- read_csv("daily_climate/siteLookup.csv")
est_1980_2019_EDI <- est_1980_2019 %>%
  left_join(siteLookup) %>%
  mutate(zone = str_extract(Zone,"^[A-Z]"),
         est_precp = round(est_daily*10,3) ) %>%
  dplyr::select(date,zone,site,est_precp,flag) %>%
  arrange(date,zone,site)

# Compare against old reference
read_csv("daily_climate/processed/JRNstudy_425001_npp_estimated_daily_precip.csv") %>%
  inner_join(est_1980_2019_EDI, by = c("date", "zone", "site")) %>%
  filter(abs(est_precp.x - est_precp.y) > 0.005)  #matches! (within rounding error)

est_1980_2019_EDI %>%
  dplyr::select(-flag) %>%
  mutate(date = paste(month(date),day(date),year(date),sep="/")) %>%
  write_csv("daily_climate/processed/gapfilled_daily_ppt_1980_2019.csv")

est_1980_2019_EDI %>%
  mutate(date = paste(month(date),day(date),year(date),sep="/")) %>%
  write_csv("daily_climate/processed/gapfilled_daily_ppt_1980_2019_flagged.csv")
