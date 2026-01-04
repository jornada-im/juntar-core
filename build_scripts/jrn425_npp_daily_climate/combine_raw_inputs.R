### Converting Darren's hourly event data to daily ppt at gauges of interest
### Dates covered Jan 1, 2014 - Aug 8, 2019 (midday)
library(tidyverse)
library(lubridate)
                                    #close to...
gaugesOfInterest <- c("Northeast",  #P-TOBO
                      "Rabbit",     #M-RABB
                      "West Well",  #M-WEST
                      "Nelson Tank",#T-WEST
                      "South Well", #T-EAST
                      "Mesquite",   #M-NORT
                      "Headquarters",#M-NORT
                      "Pasture 2",   #M-WELL
                      "Taylor Well", #T-TAYL
                      "IBP",        #G-IBPE
                      "Co-Op Well", #G-IBPE
                      "Yucca",      #P-SMAL
                      "Rep 2")      #T-TAYL

jer_hourly <- read_csv("daily_climate/raw/precip_hourly.csv") %>%
  group_by(date,gauge) %>%
  summarise(ppt_cm = 2.54*sum(precip_in)) %>%
  ungroup %>%
  filter(gauge %in% gaugesOfInterest) %>%
  complete(gauge,
           date = full_seq(as.Date(c("2014-01-01","2019-08-08")),1),
           fill = list(ppt_cm = 0)) %>%
  filter(!(gauge == "West Well" & year(date) == 2019))

jer_hourly_2019 <- read_csv("daily_climate/raw/hourly/RG-48 (West Well).dat",
                            skip = 1) %>%
  filter(!(Rain_in_Tot %in% c("inch","Tot"))) %>%
  mutate(date = as.Date(TIMESTAMP)) %>%
  filter(year(date) == 2019) %>%
  group_by(date) %>%
  summarise(ppt_cm = sum(as.numeric(Rain_in_Tot)*2.54)) %>%
  mutate(gauge = "West Well") %>%
  complete(gauge,
           date = full_seq(as.Date(c("2019-01-01","2019-12-31")),1),
           fill = list(ppt_cm = 0)) 

jer_hourly_2013 <- read_csv("daily_climate/raw/JER_TBRG_2013_selected.csv") %>%
  mutate(date = mdy(date)) %>%
  group_by(date,gauge) %>%
  summarise(ppt_cm = 2.54*sum(precip_in)) %>%
  ungroup %>%
  filter(gauge %in% gaugesOfInterest) %>%
  complete(gauge,
           date = full_seq(as.Date(c("2013-01-01","2013-12-31")),1),
           fill = list(ppt_cm = 0)) 

jin_2013 <- read_csv("daily_climate/raw/daily_inputs_2011_2013.csv") %>%
  filter(year(date) %in% c(2012,2013))

lter_2013_2019 <- read_csv("daily_climate/raw/WsLter-precip-daily_1992-2019.csv",
         skip = 15) %>%
  mutate(date = mdy(date),
         ppt_cm = ifelse(missingHours > 3,  # is that a good cutoff?
                         NA,
                         precp_mm/10),
         gauge = "LTERWS") %>%
  filter(year(date) %in% 2013:2019 & !(date %in% (jin_2013 %>% filter(gauge == "LTERWS"))$date)) %>%
  dplyr::select(date,gauge,ppt_cm)

biodiv_2013_2019 <- read_csv("daily_climate/raw/JRN_121005_biodiv_tbraingauge_daily.csv") %>%
  mutate(gauge = "BIODIV",
         ppt_cm = prec/10) %>%
  dplyr::select(date,gauge,ppt_cm)

jer_hourly %>%
  bind_rows(jer_hourly_2019) %>%
  bind_rows(jer_hourly_2013) %>%
  bind_rows(jin_2013) %>%
  bind_rows(lter_2013_2019) %>%
  bind_rows(biodiv_2013_2019) %>%
  arrange(gauge,date) %>%
  distinct() %>%
  write_csv("daily_climate/raw/nearby_daily_gauges_2013_2019.csv")
