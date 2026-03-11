### Confirming daily 2011-2013 estimations results from Jin

library(tidyverse)
library(lubridate)
library(readxl)

########
## 1. Read in raw data
########
# 1.1 Read in GRG data from the NPP sites
# grg_1989_2019 <- read_csv("ppt/raw/npp_grg.csv",
#                           skip = 93) %>%
grg_2010_2013 <- read_xlsx("daily_climate/raw/To_John_2014jun11/Input data files/npp_grg_nohead_2011_2013.xlsx") %>%
  mutate(date = as.Date(date,"%m/%d/%Y"),
         ppt_cm = as.numeric(p_mm)/10) %>%
  dplyr::select(date,site,ppt_cm) 
  

# # 1.2 Read in the daily input data
# tbrg_2013_2019 <- read_csv("daily_climate/raw/daily_climate_2013_present.csv") %>%
#   mutate(date = as.Date(date,"%m/%d/%Y"),
#          ppt_cm = as.numeric(Ppt_mm_Tot)/10) %>%
#   rename(gauge = site) %>%
#   dplyr::select(date,gauge,ppt_cm) %>%
#   complete(gauge,
#            date = full_seq(as.Date(c("2014-01-01","2018-12-31")),1),
#            fill = list(ppt_cm = NA))
# 
# # 1.3 Read in the daily nearby rain gauges
# jer_2014_2019 <- read_csv("daily_climate/raw/nearby_jer_gauges_2014_2019.csv") 

dailies <- read_csv("daily_climate/raw/daily_inputs_2011_2013.csv")

jin_results <- read_xlsx("daily_climate/raw/To_John_2014jun11/estimated_daily_ppt_jun11.xlsx") %>%
  mutate(date = as.Date(date),
         ppt_cm = as.numeric(e_daily_mm)/10) %>%
  dplyr::select(date,site,ppt_cm) 

# 1.4 Read in nearby borrowing rules
borrow <- read_csv("daily_climate/nearby_daily_gauges.csv")

########
## 2. Join data together
########

# dailies <- jer_2014_2019  %>%
#   bind_rows(tbrg_2013_2019)


getProp <- function(grg,daily = dailies){
  GAUGE <- unique(grg$gauge)
  dates <- full_seq(c(grg$date.x+1,grg$date.y),1)
  
  prop <- daily %>%
    filter(gauge == GAUGE) %>%
    filter(date %in% dates) %>%
    mutate(gauge_total = sum(ppt_cm),
           daily_prop = ifelse(gauge_total == 0,
                               0,
                               ppt_cm/gauge_total))
  
  return(prop)
}

grg_2010_2013 <- grg_2010_2013 %>%
  #filter(year(date) %in% 2014:2018) %>%
  left_join(borrow) %>%
  filter(complete.cases(.)) %>%
  arrange(site,gauge,date) %>%
  rowid_to_column() %>%
  group_by(site) %>%
  mutate(rowid = rowid - min(rowid)) %>%
  ungroup


prop_2010_2013 <- grg_2010_2013 %>%
  left_join(grg_2010_2013 %>%
              mutate(rowid=rowid-1), 
            by = c("rowid","site","gauge","rank")) %>%
  filter(!is.na(date.y))  %>%
  group_by(rowid,site) %>%
  nest() %>%
  mutate(prop = purrr::map(data, getProp)) %>%
  dplyr::select(-data) %>%
  unnest(prop) %>%
  ungroup

estimates_2010_2013 <- prop_2010_2013 %>%
  left_join(grg_2010_2013 %>%
              mutate(rowid = rowid - 1), 
            by = c("rowid", "site", "gauge")) %>%
  mutate(est_daily = ifelse(ppt_cm.y > 0 & gauge_total == 0,
                            NA,
                            daily_prop*ppt_cm.y)) %>%
  distinct() %>%  #why needed?
  dplyr::select(site,date.x,rank,est_daily) %>%
  spread(rank,est_daily) %>%
  mutate(est_daily = coalesce(`1`,`2`,`3`)) %>%
  rename(date = date.x)


# estimates_2010_2013 %>%
#   ggplot(aes(date,est_daily,fill = site)) +
#   geom_col() +
#   facet_wrap(~site)

estimates_2010_2013 %>%
  inner_join(jin_results, by = c("site", "date")) %>%
  ggplot(aes(ppt_cm,est_daily,color = site, text = date)) +
  geom_point() +
  facet_wrap(~site)


estimates_2010_2013 %>%
  inner_join(jin_results, by = c("site", "date")) %>%
  filter(ppt_cm != 0) %>%
  mutate(diff = (est_daily - ppt_cm)) %>%
  ggplot(aes(diff)) +
  geom_histogram()

estimates_2010_2013 %>%
  inner_join(jin_results, by = c("site", "date")) %>%
  filter(ppt_cm != 0 & ppt_cm != est_daily & site %in% c("NORT","RABB")) %>%
  mutate(diff = abs(est_daily - ppt_cm)) %>%
  filter(diff == max(diff))

doi <- "2011-07-24"
soi <- "NORT"
d1 <- as.Date("2011-08-03")
d2 <- as.Date("2011-06-29")

grg_2010_2013 %>%
  filter(site == soi  & date %in% c(d1,d2))

dailies %>%
  filter(gauge %in% c("NORT","RABB","Mesquite") & date %in% full_seq(c(d1,d2),1)) %>%
  filter_if(is.numeric, all_vars(. > 0))

prop_2010_2013 %>%
  filter(site == soi & date %in% full_seq(c(d1,d2),1)) %>%
  dplyr::select(-rowid) %>%
  filter_if(is.numeric, all_vars(. > 0))

prop_2010_2013 %>%
  left_join(grg_2010_2013 %>%
              mutate(rowid = rowid - 1), 
            by = c("rowid", "site", "gauge")) %>%
  filter(site == soi & date.x == doi)
