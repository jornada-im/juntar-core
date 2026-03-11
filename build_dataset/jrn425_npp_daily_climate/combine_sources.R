#### Compiling daily climate data for NPP sites 1980-2018

library(tidyverse)
library(readxl)

# 1. Read in the 1980-2010 ppt + temp records
old_files <- list.files("daily_climate/raw/daily_climate_15NPPsites_1980_2010",
                        full.names = TRUE)
p_t_1980_2010 <- tibble()
for(f in old_files){ 
  p_t_1980_2010 <- p_t_1980_2010 %>% 
    bind_rows(read_excel(f))
}
p_t_1980_2010 <- p_t_1980_2010 %>%
  mutate(date = as.Date(paste(year,month,day,sep="-")))

# 2. Read in the 2011-2013 ppt and temperature record
p_2011_2013 <- read_excel("daily_climate/raw/estimated_daily_ppt_jun11.xlsx") %>%
  mutate(date = as.Date(date))

# old_files2 <- list.files("daily_climate/raw/daily_climate_15NPPsites_2011_2012/",
#                         full.names = TRUE)
# p_t_2011_2012 <- tibble()
# for(f in old_files2){
#   p_t_2011_2012 <- p_t_2011_2012 %>%
#     bind_rows(read_excel(f))
# }
# p_t_2011_2012 <- p_t_2011_2012 %>%
#   mutate(date = as.Date(paste(year,month,day,sep="-")))

t_2011_2014 <- read_excel("daily_climate/raw/daily_temp_2011_2014mar_jun18.xlsx") %>%
  mutate(date = as.Date(date))

#Use t_2011_2014 to join temp with p_2011_2013
p_t_2011_2014 <- p_2011_2013 %>%
  left_join(t_2011_2014) %>%
  mutate(MaxT_C = ifelse(site %in% c("CALI","GRAV","SAND","SUMM"),
                         Tmax_l,
                         Tmax_w_f),
         MinT_C = ifelse(site %in% c("CALI","GRAV","SAND","SUMM"),
                         Tmin_l,
                         Tmin_w_f)) %>%
  mutate(PPT_cm = e_daily_mm/10) %>%
  dplyr::select(date,zone,site,PPT_cm,MaxT_C,MinT_C)

# # Check if it matches p_t_2011_2012
# p_t_2011_2012 %>%
#   left_join(p_t_2011_2014, by = c("site","date")) %>%
#   #filter(MaxT_C.x != MaxT_C.y)
#   #filter(MinT_C.x != MinT_C.y)
#   filter(abs(PPT_cm.x - PPT_cm.y) > 0.01)   ### so p_t_2011_2012 is not necessary


# 3. Read in the 2013+ met records
p_t_2013_2019 <- read_csv("daily_climate/raw/daily_climate_2013_present.csv")

# Any overlapping data, e.g. in 2013?
range(p_2011_2013$date)
range(p_t_2013_2019$date) #slight overlap overall
p_2011_2013 %>%
  inner_join(p_t_2013_2019, by = c("site","date")) # but doesn't look like it site-wise

# Combine all years
p_1980_2019 <- p_t_1980_2010 %>%
  dplyr::select(date,site,PPT_cm) %>%
  # bind_rows(p_2011_2013 %>%
  #             mutate(PPT_cm = e_daily_mm/10) %>%
  #             dplyr::select(date,site,PPT_cm)) %>%
  bind_rows(p_t_2011_2014 %>%
              dplyr::select(date,site,PPT_cm)) %>%
  bind_rows(p_t_2013_2019 %>%
              mutate(PPT_cm = as.numeric(Ppt_mm_Tot)/10)%>%
              dplyr::select(date,site,PPT_cm)) %>%
  complete(site, date) %>%
  arrange(date)

###### PRINT FOR NOW #######
p_1980_2019  %>%
  write.csv("daily_climate/processed/daily_ppt_1980_2019.csv",
            row.names = FALSE,
            quote = FALSE)

# Where/when is the missing data?
missing_ppt  <- p_1980_2019 %>%
  filter(is.na(PPT_cm)) 

# How many missing dates per site?
missing_ppt %>%
  group_by(site) %>%
  count()  

# Visualize missing data
missing_ppt %>%
  ggplot(aes(date,site,fill=PPT_cm)) +
  geom_tile() +
  theme_classic() +
  scale_fill_continuous(na.value = "red")
ggsave("daily_climate/figures/missing_ppt.png",
       width = 4,
       height = 4)


# Combine all years
t_1980_2019 <- p_t_1980_2010 %>%
  dplyr::select(date,site,MaxT_C,MinT_C) %>%
  # bind_rows(p_2011_2013 %>%
  #             dplyr::select(date,site)) %>%
  bind_rows(p_t_2011_2014 %>%
              dplyr::select(date,site,MaxT_C,MinT_C)) %>%
  bind_rows(p_t_2013_2019 %>%
              rename(MaxT_C = Air_TempC_Max,
                     MinT_C = Air_TempC_Min) %>%
              dplyr::select(date,site,MaxT_C,MinT_C)) %>%
  complete(site, date) %>%
  arrange(date)

###### PRINT FOR NOW #######
t_1980_2019  %>%
  write.csv("daily_climate/processed/daily_temp_1980_2019.csv",
            row.names = FALSE,
            quote = FALSE)

# Where/when is the missing data?
missing_temp  <- t_1980_2019 %>%
  filter(is.na(MaxT_C)) 

# How many missing dates per site?
missing_temp %>%
  group_by(site) %>%
  count()  

# Visualize missing data
missing_temp %>%
  ggplot(aes(date,site,fill=MaxT_C)) +
  geom_tile() +
  theme_classic() +
  scale_fill_continuous(na.value = "red")
ggsave("daily_climate/figures/missing_temp.png",
       width = 4,
       height = 4)
 


