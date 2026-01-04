library(tidyverse)
library(lubridate)

est_1980_2018 <- read.csv("daily_climate/processed/gapfilled_daily_ppt_1980_2019.csv",
                          stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(date = mdy(date),
         year = year(date))

est_1980_2018 %>%
  filter(is.na(est_precp)) %>%
  group_by(site) %>%
  count()

est_1980_2018 %>%
  filter(est_precp > 0) %>%
  ggplot(aes(est_precp, fill = factor(year))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 2) 

est_1980_2018 %>%
  filter(site %in% c("RABB","NORT","WELL")) %>%
  filter(est_precp > 0) %>%
  ggplot(aes(est_precp, fill = factor(year))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 2) +
  facet_wrap(~site) #+
  #scale_x_log10()

high <- est_1980_2018 %>%
  #filter(site %in% c("RABB","NORT","WELL")) %>%
  mutate(high = est_precp > 3 ) %>%
  group_by(site,year) %>%
  summarise(total_cm = sum(est_precp),
            high_cm = sum(est_precp*as.numeric(high)),
            high_n = sum(high),
            high_cm_prop = 100*high_cm/total_cm) 

high %>%
  ggplot(aes(year,total_cm)) +
  geom_col() +
  facet_wrap(~site) 

high %>%
  mutate(label = str_extract(year,"\\d{2}$")) %>%
  ggplot(aes(total_cm,high_cm)) +
  geom_density_2d() +
  geom_text(aes(label=label)) +
  facet_wrap(~site) 

high %>%
  ggplot(aes(year,high_cm)) +
  geom_col() +
  facet_wrap(~site) 

high_sub <- high %>%
  filter(year %in% 1981:2003)
high_new <- high %>%
  filter(year %in% 2004:2018)
high_fit <- lm(high_cm ~ total_cm, data = high_sub)
high_pred <- high_new %>%
  bind_cols(as_tibble(predict(high_fit, 
                              newdata = high_new,
                              interval = "prediction")))
high_pred %>%
  mutate(weird = high_cm < lwr & high_cm > upr) %>%
  filter(weird)


cums <- est_1980_2018 %>%
  mutate(Year = year(date)) %>%
  group_by(site,Year) %>%
  arrange(date) %>%
  mutate(cumsum = cumsum(est_precp),
         prop = cumsum/max(cumsum),
         day = yday(date)) %>%
  ungroup

cums %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(day,prop,color=Year)) +
  geom_step() +
  facet_wrap(~site)

cums %>% 
  group_by(site,Year) %>%
  filter(prop >= 0.75) %>%
  summarise(halftime = min(day)) %>%
  ggplot(aes(Year,halftime)) +
  geom_line() +
  facet_wrap(~site)

cums %>% 
  group_by(site,Year) %>%
  filter(prop >= 0.75) %>%
  summarise(halftime = min(day)) %>%
  #filter(Year %in% 1984:1988) %>%
  filter(Year %in% 2004:2008) %>%
  ungroup %>%
  summarise(mean = mean(halftime))
