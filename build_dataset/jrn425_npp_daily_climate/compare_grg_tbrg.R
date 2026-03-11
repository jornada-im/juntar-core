
library(tidyverse)
library(lubridate)

# ppt_2016 <- read_csv("daily_climate/processed/gapfilled_daily_ppt_1980_2019.csv")  %>%
#   mutate(date = mdy(date)) %>%
#   filter(year(date)  == 2016 )
# 
# ppt_2016 %>%
#   #filter(month(date) %in% 4:9)  %>%
#   dplyr::select(-zone) %>%
#   filter(site %in% c("NORT","WELL","RABB","IBPE")) %>%
#   spread(site,est_precp) %>%
#   mutate(other = RABB) %>%
#   filter(other > 0) %>%
#   ggplot(aes(WELL,other,text=date)) +
#   geom_point() +
#   geom_abline()
# plotly::ggplotly()


  
  
#### If I look at the estimated daily data for 2016+, do I see disagreement 
#### between the sites?

### Even though they are estimated, they add up to grg collections 
## and are distributed daily so I don't have to worry about different 
## collection dates. 
monthlies <- estimates_final %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year,month,site) %>%
  summarise(ppt = sum(est_daily))
  
### Every site has a highly correlated buddy when you compare monthly ppt
pairs <- monthlies %>%
  left_join(monthlies,  by = c("year", "month")) %>%
  filter(site.x != site.y) %>%
  mutate(gs = month %in% 4:9) %>%
  group_by(site.x,site.y,gs) %>%
  summarise(cor = cor(ppt.x,ppt.y)) %>%
  group_by(site.y,gs) %>%
  filter(cor == max(cor))

### Are any outliers detectable? 
grg_outliers <- monthlies %>%
  left_join(monthlies,  by = c("year", "month")) %>%
  filter(site.x != site.y) %>%
  mutate(gs = month %in% 4:9) %>%
  inner_join(pairs) %>%
  group_by(site.x,site.y,gs) %>%
  nest()  %>%
  mutate(pred = purrr::map(data, 
                           function(df){
                             baseline <<- df %>% filter(year  < 2016)
                             recent <<- df %>% filter(year  >= 2016)
                             lm(ppt.y ~ ppt.x, data=baseline) %>%
                               predict(newdata = recent,
                                       interval = "prediction",
                                       level = 0.95) %>%
                               as_tibble() %>%
                               bind_cols(recent)
                           })) %>%
  dplyr::select(-data) %>%
  unnest(pred) %>%
  mutate(outlier = (ppt.y < lwr | ppt.y > upr) & abs(ppt.y - fit) > 2,
         label = ifelse(outlier,
                        paste(site.y,"-",month.abb[month],year),
                        ""),
         season = ifelse(gs,"Growing Season", "Dormant")) 

grg_outliers %>%
  ggplot(aes(ppt.x,ppt.y,color=outlier,
             text = paste(site.y,"-",month.abb[month],year))) +
  # geom_line(aes(y = lwr,group = paste(site.x,site.y,gs)),
  #           size = 0.1,
  #           color = "gray50") + 
  # geom_line(aes(y = upr,group = paste(site.x,site.y,gs)),
  #           size = 0.1,
  #           color = "gray50") + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = label)) + 
  ggthemes::theme_few() +
  xlab("Monthly PPT at reference GRG (cm)") +
  ylab("Monthly PPT at target GRG (cm)") +
  facet_wrap(~season,
             nrow = 2) +
  scale_color_discrete(guide = FALSE)
#plotly::ggplotly()
ggsave("daily_climate/figures/grg_monthly_outliers.png",
       type = "cairo",
       width = 7.5,
       height = 10)

### What about at the GS level? Not really enough data


## Now what happens if you do the same thing with TBRG data?
## Just omit months with missing data but keeping the NA in the monthly sums

tbrg_monthlies <- read_csv("daily_climate/raw/daily_climate_2013_present.csv") %>%
  mutate(date = as.Date(date,"%m/%d/%Y"),
         ppt_cm = ifelse(Flag_Ppt_mm_Tot == "A",as.numeric(Ppt_mm_Tot)/10,NA)) %>%
  mutate(year = year(date),
       month = month(date)) %>%
  group_by(year,month,site) %>%
  summarise(ppt = sum(ppt_cm)) %>%
  ungroup

tbrg_pairs <- tbrg_monthlies %>%
  left_join(tbrg_monthlies,  by = c("year", "month")) %>%
  filter(site.x != site.y) %>%
  filter(complete.cases(.)) %>%
  mutate(gs = month %in% 4:9) %>%
  group_by(site.x,site.y,gs) %>%
  summarise(cor = cor(ppt.x,ppt.y)) %>%
  group_by(site.y,gs) %>%
  filter(cor == max(cor))
## a bit  different from grg pairs

tbrg_m_outliers <- tbrg_monthlies %>%
  left_join(tbrg_monthlies,  by = c("year", "month")) %>%
  filter(site.x != site.y) %>%
  filter(complete.cases(.)) %>%
  mutate(gs = month %in% 4:9) %>%
  inner_join(tbrg_pairs) %>%
  group_by(site.x,site.y,gs) %>%
  nest()  %>%
  mutate(pred = purrr::map(data,
                           function(df){
                             baseline <<- df #%>% filter(year  < 2018)
                             recent <<- df #%>% filter(year  >= 2018)
                             lm(ppt.y ~ ppt.x, data=baseline) %>%
                               predict(newdata = recent,
                                       interval = "prediction",
                                       level = 0.95) %>%
                               as_tibble() %>%
                               bind_cols(recent)
                           })) %>%
  dplyr::select(-data) %>%
  unnest(pred) %>%
  mutate(outlier = (ppt.y < lwr | ppt.y > upr) & abs(ppt.y - fit) > 2,
         label = ifelse(outlier,
                        paste(site.y,"-",month.abb[month],year),
                        ""),season = ifelse(gs,"Growing Season", "Dormant"))

tbrg_m_outliers %>%
  ggplot(aes(ppt.x,ppt.y,color=outlier,
             text = paste(site.y,"-",month.abb[month],year))) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = label)) +
  ggthemes::theme_few() +
  xlab("Monthly PPT at reference TBRG (cm)") +
  ylab("Monthly PPT at target TBRG (cm)") +
  facet_wrap(~season,
             nrow = 2) +
  theme(legend.position = "none")
ggsave("daily_climate/figures/tbrg_monthly_outliers.png",
       type = "cairo",
       width = 7.5,
       height = 10)

grg_tbrg_discrepancies <- estimates_final %>%
  inner_join(tbrg_2013_2019, by = c("site" = "gauge", "date")) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  filter(year %in% 2016:2019) %>%
  group_by(year,month,site) %>%
  summarise(ppt_cm = sum(ppt_cm),
            est_monthly = sum(est_daily)) %>%
  mutate(raw_diff = ppt_cm-est_monthly,
         perc_diff = 100*(raw_diff)/est_monthly,
         outlier = abs(perc_diff) > 20 & abs(raw_diff) > 2, 
         label = ifelse(outlier, 
                        paste(#site,"-",
                              month.abb[month],year), ""),
         pos = ifelse(raw_diff, "Below", "Above")) 

grg_tbrg_discrepancies  %>%
  ggplot(aes(est_monthly,ppt_cm,color = outlier, text = paste(year,"-",month))) +
  geom_abline() + 
  geom_point(show.legend = FALSE) +
  ggrepel::geom_text_repel(aes(label = label),
                           show.legend = FALSE) + 
  facet_wrap(~site) +
  ggthemes::theme_few() +
  xlab("Monthly Estimated PPT from GRG (cm)") +
  ylab("Monthly sum bbserved TBRG PPT (cm)") 
#plotly::ggplotly()
ggsave("daily_climate/figures/grg_monthly_tbrg.png",
       type = "cairo",
       width = 7.5,
       height = 10)


##### TBRG DAILY COMPARISON
tbrg_dailies <- read_csv("daily_climate/raw/daily_climate_2013_present.csv") %>%
  mutate(date = as.Date(date,"%m/%d/%Y"),
         ppt = ifelse(Flag_Ppt_mm_Tot == "A",as.numeric(Ppt_mm_Tot)/10,NA),
         year = year(date),
         month = month(date)) %>%
  filter(Flag_Ppt_mm_Tot == "A")

pairs <- tbrg_dailies %>%
  left_join(tbrg_dailies,  by = c("date","year","month")) %>%
  filter(site.x != site.y) %>%
  mutate(gs = month %in% 4:9) %>%
  group_by(site.x,site.y,gs) %>%
  summarise(cor = cor(ppt.x,ppt.y)) %>%
  group_by(site.y,gs) %>%
  filter(cor == max(cor))



tbrg_outliers <- tbrg_dailies %>%
  left_join(tbrg_dailies,  by = c("date","year", "month")) %>%
  filter(site.x != site.y) %>%
  mutate(gs = month %in% 4:9) %>%
  inner_join(pairs) %>%
  group_by(site.x,site.y,gs) %>%
  nest()  %>%
  mutate(pred = purrr::map(data, 
                           function(df){
                             baseline <<- df #%>% filter(year  < 2018)
                             recent <<- df #%>% filter(year  >= 2018)
                             lm(ppt.y ~ ppt.x, data=baseline) %>%
                               predict(newdata = recent,
                                       interval = "prediction",
                                       level = 0.95) %>%
                               as_tibble() %>%
                               bind_cols(recent)
                           })) %>%
  dplyr::select(-data) %>%
  unnest(pred) %>%
  mutate(outlier = (ppt.y < lwr | ppt.y > upr) & abs(ppt.y - fit) > 0.5,
         label = ifelse(outlier,
                        paste(site.y,"-",month.abb[month],year),
                        ""),
         season = ifelse(gs,"Growing Season", "Dormant")) 

tbrg_outliers %>%
  #filter(site.y == "BASN") %>%
  ggplot(aes(ppt.x,ppt.y,color=outlier,
             text = paste(site.y,"-",month.abb[month],year))) +
  # geom_line(aes(y = lwr,group = paste(site.x,site.y,gs)),
  #           size = 0.1,
  #           color = "gray50") + 
  # geom_line(aes(y = upr,group = paste(site.x,site.y,gs)),
  #           size = 0.1,
  #           color = "gray50") + 
  geom_abline() +
  geom_point() +
  #ggrepel::geom_text_repel(aes(label = label)) + 
  ggthemes::theme_few() +
  xlab("Daily PPT at reference TBRG (cm)") +
  ylab("Daily PPT at target TBRG (cm)") +
  facet_wrap(~season,
             nrow = 2) +
  theme(legend.position = "none")
ggsave("daily_climate/figures/tbrg_daily_outliers.png",
       type = "cairo",
       width = 7.5,
       height = 10)

#plotly::ggplotly()
# ggsave("daily_climate/figures/tbrg_daily_outliers.png",
#        type = "cairo",
#        width = 7.5,
#        height = 10)

#### Find the site/months where the grg-tbrg disagee and see if
#### that is also when there are daily tbrg outliers

discrep <- grg_tbrg_discrepancies %>%
  filter(nchar(label) > 0) %>%
  distinct(year,month,site)
## 32 discrepancies

grg_out <- grg_outliers %>%
  ungroup %>%
  filter(nchar(label) > 0) %>%
  rename(site = site.y) %>%
  distinct(year,month,site)

tbrg_m_out <- tbrg_m_outliers %>%
  ungroup %>%
  filter(nchar(label) > 0) %>%
  rename(site = site.y) %>%
  distinct(year,month,site)

tbrg_out <- tbrg_outliers %>%
  ungroup %>%
  filter(nchar(label) > 0) %>%
  rename(site = site.y) %>%
  distinct(year,month,site) %>%
  filter(year %in% 2016:2019)
## 170 months with at least one daily outlier (0.5 threshold)


## How many of the 32 discrepancies could be due to tbrg outliers?
discrep %>%
  inner_join(tbrg_out) 
## 20 of them

## How many of the 32 discrepancies could be due to grg outliers?
discrep %>%
  inner_join(grg_out) 
## none

## How many of the 32 discrepancies could be due to monthly tbrg outliers?
discrep %>%
  inner_join(tbrg_m_out) 
## none

## Remaining:
discrep %>%
  anti_join(tbrg_out) 

######## Visualizing
estimates_final %>%
  filter(site == "WELL" & year(date) == 2016 & month(date) %in% 4:9) %>%
  arrange(date) %>%
  dplyr::select(-rowid) %>%
  left_join(tbrg_dailies %>%
              dplyr::select(date,site,ppt)) %>%
  gather(sensor, ppt, est_daily:ppt) %>%
  group_by(site,sensor) %>%
  mutate(ppt_cum = cumsum(ppt)) %>%
  ggplot(aes(date,ppt_cum,color=sensor)) +
  geom_step()

questionable <- discrep %>%
  anti_join(tbrg_out) %>%
  rowid_to_column("case")

prop_2013_2019 %>%
  filter(site == gauge & complete.cases(.)) %>%
  left_join(grg_2013_2019 %>%
              mutate(rowid = rowid - 1), 
            by = c("rowid", "site", "gauge")) %>%
  rename(grg = ppt_cm.y,
         tbrg = ppt_cm.x,
         date = date.x) %>%
  mutate(grg = ifelse(date == date.y, grg, 0)) %>%
  #filter(month(date) %in% 4:9) %>%
  arrange(date) %>%
  dplyr::select(date,site,tbrg,grg) %>%
  gather(sensor, ppt, tbrg:grg) %>%
  group_by(site,sensor) %>%
  mutate(ppt_cum = cumsum(ppt)) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  ungroup %>%
  inner_join(questionable, by = c("site", "year")) %>% 
  ggplot(aes(date,ppt_cum,
             color=sensor,
             group=paste(site,sensor))) +
  geom_step() +
  ggthemes::theme_few() +
  facet_wrap(~case)





########## Finally getting towards how to handle suspicious daily climate

# Step 1. Check tbrg daily data against its peers to see if it is suspicious.

tbrg_dailies <- read_csv("daily_climate/raw/daily_climate_2013_present.csv") %>%
  mutate(date = as.Date(date,"%m/%d/%Y"),
         ppt = ifelse(Flag_Ppt_mm_Tot == "A",as.numeric(Ppt_mm_Tot)/10,NA),
         year = year(date),
         month = month(date)) %>%
  filter(Flag_Ppt_mm_Tot == "A")

tbrg_d_pairs <- tbrg_dailies %>%
  left_join(tbrg_dailies,  by = c("date","year","month")) %>%
  filter(site.x != site.y) %>%
  mutate(gs = month %in% 4:9) %>%
  group_by(site.x,site.y,gs) %>%
  summarise(cor = cor(ppt.x,ppt.y)) %>%
  group_by(site.y,gs) %>%
  filter(cor == max(cor))

tbrg_d_outliers <- tbrg_dailies %>%
  left_join(tbrg_dailies,  by = c("date","year", "month")) %>%
  filter(site.x != site.y) %>%
  mutate(gs = month %in% 7:10) %>%
  inner_join(tbrg_d_pairs) %>%
  group_by(site.x,site.y,gs) %>%
  nest()  %>%
  mutate(pred = purrr::map(data, 
                           function(df){
                             baseline <<- df #%>% filter(year  < 2018)
                             recent <<- df #%>% filter(year  >= 2018)
                             lm(ppt.y ~ ppt.x, data=baseline) %>%
                               predict(newdata = recent,
                                       interval = "prediction",
                                       level = 0.95) %>%
                               as_tibble() %>%
                               bind_cols(recent)
                           })) %>%
  dplyr::select(-data) %>%
  unnest(pred) %>%
  mutate(outlier = (ppt.y < lwr | ppt.y > upr))  %>%
  filter(year <= 2019)

tbrg_d_outliers %>%
  filter(year %in% 2016:2019) %>%
  filter(outlier) %>%
  group_by(date) %>%
  count() %>%
  ggplot(aes(date,n)) +
  geom_col() +
  ggthemes::theme_few() +
  scale_y_continuous(limits = c(0,15),
                     expand = c(0,0))

tbrg_d_outliers %>%
  group_by(year,outlier) %>%
  count() %>%
  spread(outlier,n) %>%
  mutate(percent = 100*`TRUE`/(`TRUE` + `FALSE`))
### So <4.75% of cases per year are suspicious at a 99% level  (though kind of handwavy)
### So <6.25% of cases per year are suspicious at a 95% level  (though kind of handwavy)

# Step 2. For those suspicious TBRG days, do they agree with grg months? 
estimates_final %>%
  inner_join(tbrg_d_outliers %>%
               filter(outlier), by = c("site" = "site.y", "date")) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year,month,site) %>%
  summarise(ppt_cm = sum(ppt.y),
            est_monthly = sum(est_daily)) %>%
  mutate(raw_diff = ppt_cm-est_monthly,
         perc_diff = 100*(raw_diff)/est_monthly,
         outlier = abs(perc_diff) > 20) 



estimates_final %>%
  mutate(year = year(date)) %>%
  filter(est_daily > 0 & year %in% 2013:2019) %>%
  ggplot(aes(est_daily,color=factor(year))) +
  geom_density() +
  scale_x_log10() +
  facet_wrap(~site)
  
  
  
