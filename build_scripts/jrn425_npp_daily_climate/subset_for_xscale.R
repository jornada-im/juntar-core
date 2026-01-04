### Segmenting daily climate for xscale:

library(tidyverse)
library(lubridate)

## Gauges: WELL, RABB, COLL, IBPE
## Time frame: 2010 - 2019

ppt_f <- "daily_climate/processed/gapfilled_daily_ppt_1980_2019.csv"
all_ppt <- read_csv(ppt_f) %>%
  mutate(date = mdy(date))

all_ppt %>%
  filter(year(date) %in% 2010:2019 & 
           site %in% c("WELL", "RABB", "COLL", "IBPE")) %>%
  mutate(ppt_cm = est_precp/10) %>%
  rename(gauge = site) %>%
  dplyr::select(date,gauge,ppt_cm) %>%
  write_csv("../xscale/data/raw/nearby_ppt_2010_2019.csv")

### GRG data as well
grg_f <- "ppt/processed/PPT_monthly.csv"
all_grg <- read_csv(grg_f) %>%
  filter(year %in% 2010:2019 & 
           site %in% c("WELL", "RABB", "COLL", "IBPE")) %>%
  rename(gauge = site,
         ppt_cm = p_cm) %>%
  dplyr::select(year,month,gauge,ppt_cm) %>%
  write_csv("../xscale/data/raw/nearby_ppt_monthly_2010_2019.csv")
