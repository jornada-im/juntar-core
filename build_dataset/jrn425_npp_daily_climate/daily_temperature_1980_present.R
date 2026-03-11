#### Estimating daily temperature 1980-present

library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
  
######
### 1. Read in the reference data from LTER and HQ from 1980 - 2017  [UPDATE AS MUCH AS POSSIBLE]
######
old_files <- list.files("daily_climate/raw/daily_climate_15NPPsites_1980_2010",
                        full.names = TRUE)
p_t_1980_2010 <- tibble()
for(f in old_files){
  p_t_1980_2010 <- p_t_1980_2010 %>%
    bind_rows(read_excel(f) %>%
                mutate(date = as.Date(paste(year,month,day,sep="-"))))
}
t_1980_2010 <- p_t_1980_2010 %>%
  dplyr::select(date,site,MaxT_C,MinT_C) 
t_2011_2014 <- read_excel("daily_climate/raw/daily_temp_2011_2014mar_jun18.xlsx") %>%
  mutate(date = as.Date(date)) 

# Updated files since Jin's last 2014 analysis
lterws_2013_2019 <- read_table("daily_climate/raw/WSDAY_airTemp_2013-2019.txt",
           skip = 17) %>%
  mutate(date = mdy(Date)) %>%
  dplyr::select(date,MaxAir,MinAir) 

hq_2014_2019 <- read_csv("daily_climate/raw/hq_noaa_p_t_2014_2019.csv") %>%
  dplyr::select(date, Ta_min, Ta_max)
updated_ref <- lterws_2013_2019 %>%
  full_join(hq_2014_2019) %>%
  arrange(date) %>%
  complete(date = full_seq(date,1)) %>%
  distinct()

### Gap fill refs with each other like Jin did
min_l2h_fit <- lm(Ta_min ~ MinAir, data = updated_ref )
max_l2h_fit <- lm(Ta_max ~ MaxAir, data = updated_ref )
  
filled_updated_ref <- updated_ref %>%
  filter(year(date) > 2013 & !complete.cases(.)) %>%
  group_by(date) %>%
  nest(data=c(MinAir,MaxAir)) %>%
  mutate(Ta_min = purrr::map(data,function(nd){predict(min_l2h_fit, newdata = nd)}),
         Ta_max = purrr::map(data,function(nd){predict(max_l2h_fit, newdata = nd)})
         ) %>%
  unnest(c(Ta_min,Ta_max,data)) %>%
  ungroup %>%
  bind_rows(updated_ref %>%
              filter(!(year(date) > 2013 & !complete.cases(.)))) %>%
  arrange(date)



# Combine all reference data
ref_temp_1980_2019 <- t_1980_2010 %>%
  complete(site, date = full_seq(c(date,max(filled_updated_ref$date)),1)) %>%
  left_join(t_2011_2014) %>%
  mutate(MaxT_C = ifelse(is.na(MaxT_C) & site %in% c("CALI","GRAV","SAND","SUMM"),
                         Tmax_l,
                         ifelse(is.na(MaxT_C) & !(site %in% c("CALI","GRAV","SAND","SUMM")),
                                Tmax_w_f,
                                MaxT_C)),
         MinT_C = ifelse(is.na(MinT_C) & site %in% c("CALI","GRAV","SAND","SUMM"),
                         Tmin_l,
                         ifelse(is.na(MinT_C) & !(site %in% c("CALI","GRAV","SAND","SUMM")),
                                Tmin_w_f,
                                MinT_C))) %>%
  dplyr::select(site:MinT_C) %>%
  left_join(filled_updated_ref) %>%
  mutate(MaxT_C = ifelse(is.na(MaxT_C) & site %in% c("CALI","GRAV","SAND","SUMM"),
                         MaxAir,
                         ifelse(is.na(MaxT_C) & !(site %in% c("CALI","GRAV","SAND","SUMM")),
                                Ta_max,
                                MaxT_C)),
         MinT_C = ifelse(is.na(MinT_C) & site %in% c("CALI","GRAV","SAND","SUMM"),
                         MinAir,
                         ifelse(is.na(MinT_C) & !(site %in% c("CALI","GRAV","SAND","SUMM")),
                                Ta_min,
                                MinT_C))) %>%
  dplyr::select(site:MinT_C) %>%
  gather(var,x,-c(site,date)) %>%
  arrange(date)


ref_temp_1980_2019 %>%
  filter(!complete.cases(.)) %>%
  distinct(date)

######
### 2. Read in the wireless sensor data from mid 2013+
######
wireless_2013_2019 <- read_csv("daily_climate/raw/daily_climate_2013_present.csv") %>%
  dplyr::select(-Ppt_mm_Tot, -contains("Flag")) %>%
  rename(MinT_C = Air_TempC_Min,
         MaxT_C = Air_TempC_Max) %>%
  gather(var,y,-c(date,site))


xy_overlap <- wireless_2013_2019 %>%
  inner_join(ref_temp_1980_2019) 

######
### 3. Build model per site and season and temo type
######
fits <- xy_overlap %>%
  mutate(season = ifelse(as.numeric(format(date,"%m")) %in% 4:9, "gs","nongs")) %>%
  filter(!is.na(y)) %>%
  group_by(site,season,var) %>%
  nest() %>%
  mutate(fit = purrr::map(data,function(df){lm(y~x,data=df)})) %>%
  dplyr::select(-data)


######
### 4. Predict site temperature from ref
######
pred_1980_2019 <- ref_temp_1980_2019 %>%  
  mutate(season = ifelse(as.numeric(format(date,"%m")) %in% 4:9, "gs","nongs")) %>%
  group_by(season,var,site) %>%
  nest() %>%
  left_join(fits) %>%
  mutate(pred = purrr::map2(data,fit,function(newdata,fit){predict(fit,newdata)})) %>%
  dplyr::select(-fit) %>%
  unnest(c(data,pred)) 

pred_1980_2019  %>% 
  mutate(ym = as.yearmon(date)) %>%
  group_by(ym,site,var) %>%
  summarise(temp = mean(pred, na.rm = TRUE)) %>%
  ggplot(aes(ym,temp)) + 
  geom_line(aes(color = var)) +
  facet_wrap(~site) +
  theme_bw()

temp_1980_2019 <- pred_1980_2019 %>%
  full_join(wireless_2013_2019) %>%
  distinct() %>%
  mutate(temp = coalesce(y,pred),
         kind = ifelse(temp == y & !is.na(y), "Observed", "Estimated")) %>%
  ungroup %>%
  dplyr::select(var,date,site,temp,kind) %>%
  filter(year(date) %in% 1980:2018) %>%
  complete(date = full_seq(date,1),site,var) 


temp_1980_2019 %>%
  mutate(ym = as.yearmon(date)) %>%
  group_by(ym,site,kind,var) %>%
  summarise(temp = mean(temp, na.rm = TRUE)) %>%
  ggplot(aes(ym,temp)) +
  geom_line(aes(linetype = kind,
                color = var)) +
  facet_wrap(~site) +
  theme_bw()

temp_1980_2019 %>%
  dplyr::select(-kind) %>%
  spread(var,temp) %>%
  write_csv("daily_climate/processed/est_daily_temp_1980_2018.csv")



# # Summarize annual values
# temp_annual <- temp_monthly %>%
#   group_by(Year,site) %>%
#   summarise(kind = ifelse(sum(kind == "Observed") > 6, #| unique(Year) > 2013, 
#                           "Observed", 
#                           "Estimated"),
#             Temperature = min(temp))  
# 
# # 6. Visually inspect
# temp_annual %>%
#   ggplot(aes(Year,Temperature,color=site)) +
#   geom_line() +
#   geom_point(aes(shape = kind)) +
#   #facet_wrap(~site) +
#   scale_shape_manual(values = c(1,16))
# 
# # Write annual summary to file
# temp_annual %>%
#   dplyr::select(-kind) %>%
#   write.csv("daily_climate/processed/daily_temp_1980_2019.csv",
#             row.names = FALSE,
#             quote = FALSE)



