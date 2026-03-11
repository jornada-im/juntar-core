### Estimating daily ppt at NPP sites based on tbrg and nearby rain gauges

library(tidyverse)
library(lubridate)

########
## 1. Read in raw data
########
# 1.1 Read in GRG data from the NPP sites
grg_1989_2019 <- read_csv("ppt/raw/GRG-network-data.csv",
                          skip = 2) %>%
  mutate(date = mdy(date),
         ppt_cm = as.numeric(ppt_mm)/10) %>%
  filter(!is.na(date) & !(site %in% c("P12A","NFLM","SFLM","UPTR","BIOD","SMLM")) ) %>%
  dplyr::select(date,site,ppt_cm)

# 1.2 Read in the CORRECTED daily tbrg data from the NPP sites
# tbrg_2013_2019 <- read_csv("daily_climate/raw/daily_climate_2013_present.csv") %>%
#   mutate(date = as.Date(date,"%m/%d/%Y"),
#          ppt_cm = ifelse(Flag_Ppt_mm_Tot == "A",as.numeric(Ppt_mm_Tot)/10,NA)) %>%
#   rename(gauge = site) %>%
#   dplyr::select(date,gauge,ppt_cm) %>%
#   complete(gauge,
#            date = full_seq(date,1),
#            fill = list(ppt_cm = NA))

tbrg_2013_2019 <- read_csv("daily_climate/processed/corrected_daily_tbrg.csv") %>%
  rename(gauge = site) %>%
  complete(gauge,
           date = full_seq(date,1),
           fill = list(ppt_cm = NA))

# 1.3 Read in the daily nearby rain gauges
nearby_2013_2019 <- read_csv("daily_climate/raw/nearby_daily_gauges_2013_2019.csv") 

# 1.4 Read in nearby borrowing rules
borrow <- read_csv("daily_climate/daily_estimate_ppt_gauge_priorities.csv") 

########
## 2. Join data together
########

dailies <- nearby_2013_2019  %>%
  full_join(tbrg_2013_2019, by = c("gauge", "date")) %>% 
  mutate(ppt_cm = round(coalesce(ppt_cm.y,ppt_cm.x),3))  %>%
  dplyr::select(gauge,date,ppt_cm) %>% 
  distinct()

## This function takes in grg collection data and a daily ppt source
## and figures out how to distribute the grg data proportional to
## the daily data
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

## Join borrowing rules to grg observations so all candidates can be considered
grg_2013_2019 <- grg_1989_2019 %>%
  left_join(borrow) %>%
  #filter(complete.cases(.)) %>%  
  arrange(site,gauge,date) %>%
  rowid_to_column() %>%
  group_by(site) %>%
  mutate(rowid = rowid - min(rowid)) %>%
  ungroup

# Calculate the proportion of rain for the grg collection period 
# at each gauge 
prop_2013_2019 <- grg_2013_2019 %>%
  left_join(grg_2013_2019 %>%
              mutate(rowid=rowid-1), 
            by = c("rowid","site","gauge","priority")) %>%
  filter(!is.na(date.y))  %>%
  group_by(rowid,site) %>%
  nest() %>%
  mutate(prop = purrr::map(data, getProp)) %>%
  dplyr::select(-data) %>%
  unnest(prop) %>%
  ungroup

# How different are the collection totals between grg and tbrg?
prop_2013_2019 %>%
  filter(site == gauge) %>%
  left_join(grg_2013_2019 %>%
              mutate(rowid = rowid - 1), 
            by = c("rowid", "site", "gauge")) %>%
  distinct(site,date.y,gauge_total,ppt_cm.y) %>%
  mutate(diff = gauge_total - ppt_cm.y) %>%
  ggplot(aes(diff)) +
  geom_histogram() +
  facet_wrap(~site)

# diffs <- prop_2013_2019 %>%
#   filter(site == gauge) %>%
#   left_join(grg_2013_2019 %>%
#               mutate(rowid = rowid - 1), 
#             by = c("rowid", "site", "gauge")) %>%
#   distinct(site,date.y,gauge_total,ppt_cm.y) %>%
#   filter(!is.na(gauge_total)) %>%
#   mutate(raw_diff = gauge_total-ppt_cm.y,
#          perc_diff = 100*raw_diff/ppt_cm.y,
#          year = year(date.y),
#          month = month(date.y)) %>%
#   #filter(month(date.y) %in% c(4:9) ) %>%
#   filter(year %in% 2016:2019) %>%
#   mutate(outlier = abs(perc_diff) > 75,
#          label = ifelse(outlier, 
#                         paste(month.abb[month],year), ""),
#          pos = ifelse(ppt_cm.y < gauge_total, "Below", "Above")) %>%
#   left_join(read_csv("daily_climate/siteLookup.csv")) 

# diffs %>%
#   ggplot(aes(perc_diff))  + 
#   geom_histogram(aes(y = ..density..),
#                  binwidth = 10,
#                  fill = "grey",
#                  color="black") +
#   geom_density() + 
#   geom_vline(xintercept = c(-75, 75)) + 
#   ggthemes::theme_few() +
#   xlab("Percent difference: TBRG daily sum vs GRG collection")
# ggsave("daily_climate/figures/grg_collection_tbrg_histogram.png",
#        type = "cairo",
#        width = 7.5,
#        height = 10)

# diffs %>%
#   ggplot(aes(ppt_cm.y,gauge_total,color=outlier)) +
#   geom_point() +
#   # ggrepel::geom_text_repel(aes(label = label),
#   #                          show.legend = FALSE) +
#   geom_abline() +
#   ggthemes::theme_few() + 
#   facet_wrap(~site) +
#   xlab("GRG Collection (cm)") +
#   ylab("TBRG sum over collection period (cm)")  +
#   theme(legend.position = "none")
# ggsave("daily_climate/figures/grg_collection_tbrg.png",
#        type = "cairo",
#        width = 7.5,
#        height = 10)



paired_gauges <- prop_2013_2019 %>%
  left_join(grg_2013_2019 %>%
              mutate(rowid = rowid - 1), 
            by = c("rowid", "site", "gauge")) %>%
  mutate(est_daily = ifelse(ppt_cm.y > 0 & gauge_total == 0, # grg > 0 but daily gauge sum = 0
                            NA, # don't use this gauge
                            ifelse(is.na(ppt_cm.y), # if no grg data to distribute
                                   ppt_cm.x, # use observed daily gauge
                                   daily_prop*ppt_cm.y))) #go ahead an use daily proportion times grg total

estimates_2013_2019 <- paired_gauges %>% 
  dplyr::select(site,date.x,priority,est_daily) %>%
  spread(priority,est_daily) %>%
  mutate(est_daily = coalesce(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`)) %>% 
  rename(date = date.x)
  

#### CHECK FOR THOSE NA FROM TINY GRG AND ZERO TBRG
na_est <- estimates_2013_2019 %>%
  filter(year(date) >= 2013) %>%
  filter(is.na(est_daily))  %>%
  dplyr::select(site,date)

overwrite_small_grg <- prop_2013_2019 %>%
  filter(site == gauge) %>%
  left_join(grg_2013_2019 %>%
              mutate(rowid = rowid - 1), 
            by = c("rowid", "site", "gauge")) %>%  #all paired gauges by date
  rename(date = date.x) %>%
  inner_join(na_est) %>%
  group_by(site,gauge,date.y) %>%
  mutate(est_daily = ifelse(sum(gauge_total, na.rm = TRUE) == 0 & unique(ppt_cm.y) < 0.25,
                               0,
                               NA)) %>%
  ungroup %>%
  filter(est_daily == 0) %>%
  dplyr::select(date,site,est_daily)

estimates_final <- estimates_2013_2019 %>%
  filter(year(date) >= 2013) %>%
  filter(!is.na(est_daily)) %>%
  dplyr::select(date,site,est_daily) %>%
  bind_rows(overwrite_small_grg) %>%
  complete(date = full_seq(as.Date(c("2013-01-01","2019-12-31")),1),
           site) %>%
  arrange(site,date) %>%
  left_join(prop_2013_2019 %>% 
              filter(site == gauge) %>% 
              distinct(date,site,rowid))  #Adds grg collection period ID

### CHECK FOR discrepancies in total estimated versus grg
paired_gauges %>%
  rename(date = date.x) %>%
  filter(!is.na(est_daily)) %>%
  group_by(site,date.y) %>%
  filter(priority == min(priority)) %>%
  inner_join(estimates_2013_2019) %>% 
  group_by(site,date.y,ppt_cm.y) %>%
  filter(abs(sum(est_daily) - ppt_cm.y) >= 0.1)   # no one above 1mm diff

estimates_final %>%
  inner_join(tbrg_2013_2019, by = c("site" = "gauge", "date")) %>%
  ggplot(aes(ppt_cm,est_daily,color = site)) +
  geom_abline(slope=1,intercept=0) + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~site) +
  theme_bw() +
  ylab("Estimated PPT (cm)") +
  xlab("Observed TBRG PPT (cm)")

estimates_final %>%
  inner_join(tbrg_2013_2019, by = c("site" = "gauge", "date"))  %>%
  filter(is.na(ppt_cm))

estimates_final %>%
  inner_join(tbrg_2013_2019, by = c("site" = "gauge", "date")) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  filter(year %in% 2016:2018) %>%
  group_by(year,month,site) %>%
  summarise(ppt_cm = sum(ppt_cm),
            est_monthly = sum(est_daily)) %>%
  ggplot(aes(est_monthly,ppt_cm,color = site, text = paste(year,"-",month))) +
  geom_abline() + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~site) +
  ggthemes::theme_few() +
  xlab("Estimated PPT (cm)") +
  ylab("Observed TBRG PPT (cm)")
plotly::ggplotly()

estimates_final %>%
  inner_join(tbrg_2013_2019, by = c("site" = "gauge", "date")) %>%
  filter(year(date) %in% 2016:2018) %>%
  filter(month(date)%in% 4:9) %>%
  group_by(year(date),site) %>%
  summarise(ppt_cm = sum(ppt_cm),
            est_daily = sum(est_daily)) %>%
  ggplot(aes(est_daily,ppt_cm,color = site)) +
  geom_abline() + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~site) +
  ggthemes::theme_few() +
  xlab("Estimated PPT (cm)") +
  ylab("Observed TBRG PPT (cm)")

estimates_final %>%
  write_csv("daily_climate/processed/est_daily_ppt_2013_2019.csv") 

