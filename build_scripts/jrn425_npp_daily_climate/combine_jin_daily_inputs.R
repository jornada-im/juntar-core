#### Collect Jin's input files from her 2011 - mid 2013 daily ppt estimation
#### for checking my logic for 2013+ daily estimation

library(tidyverse)
library(lubridate)
library(xlsx)

### 1. The NPP tbrg (former logger) 
npp_tbrg_f <- list.files("daily_climate/raw/To_John_2014jun11/Input data files/",
           "ppt_daily_flags*",
           full.names = TRUE)

tbrg_2010_2013 <- tibble()
for(file in npp_tbrg_f){
  tbrg_2010_2013 <- tbrg_2010_2013 %>% 
    bind_rows(read_csv(file) %>%
                rename(date = date2,
                       gauge = site) %>%
                mutate(ppt_cm = pptmm2/10,
                       ppt_cm = ifelse(hr_nonfunc2 > 0,
                              NA,
                              ppt_cm)) %>%
                dplyr::select(date,gauge,ppt_cm))
  
}
tbrg_2011_2013 <- tbrg_2010_2013 %>%
  filter(year(date) %in% 2011:2013) 
 
 
### 2. LTER weather station
lter_2011_2013 <- read_xlsx("daily_climate/raw/To_John_2014jun11/Input data files/ppt_daily_LTER_met_2011_2013.xlsx") %>%
  mutate(date = as.Date(Date),
         ppt_cm = Ppt_mm/10,
         gauge = "LTERWS") %>%
  dplyr::select(date,gauge,ppt_cm) %>%
  filter(year(date) %in% 2011:2013) %>%
  complete(gauge,
           date = full_seq(as.Date(c("2011-01-01","2013-12-31")),1),
           fill = list(ppt_cm = 0))

### 3. JER Automated gauges
jer_2011_2013 <- read_csv("daily_climate/raw/To_John_2014jun11/Input data files/ppt_daily.csv") %>%
  mutate(date = as.Date(date,"%m/%d/%y"),
         ppt_cm = pptmm/10) %>%
  dplyr::select(date,gauge,ppt_cm) %>%
  filter(year(date) %in% 2011:2013) %>%
  complete(gauge = "Northeast",
           date = full_seq(as.Date(c("2011-01-01","2013-12-31")),1),
           fill = list(ppt_cm = 0)) %>%
  complete(gauge = c("Rabbit","West Well"),
           date = full_seq(as.Date(c("2012-01-01","2013-12-31")),1),
           fill = list(ppt_cm = 0)) %>%
  complete(gauge = "Mesquite",
           date = full_seq(as.Date(c("2011-01-01","2012-03-19")),1),
           fill = list(ppt_cm = 0))

### 4. CDRRC gauges
cdrrc_2011_2013 <- read_xlsx("daily_climate/raw/To_John_2014jun11/Input data files/ppt_2011_2013jun.xlsx") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(date,VMPL,EMTS) %>%
  gather(gauge,ppt_cm,c(VMPL,EMTS)) %>%
  complete(gauge,
           date = full_seq(as.Date(c("2011-01-01","2013-06-30")),1),
           fill = list(ppt_cm = 0))

### 5. Stressor weighing gauge
stressorW_2011_2012 <- read_xlsx("daily_climate/raw/To_John_2014jun11/Input data files/Stressor_I_west 2011.xlsx") %>%
  mutate(date = as.Date(date),
         ppt_cm = pptmm/10) %>%
  mutate(gauge = "STR1W") %>%
  dplyr::select(date,gauge,ppt_cm) %>%
  complete(gauge,
           date = full_seq(as.Date(c("2011-01-01","2012-01-10")),1),
           fill = list(ppt_cm = 0))

stressorE_2011_2011 <- read_xlsx("daily_climate/raw/To_John_2014jun11/Input data files/Stressor 1_east 2011.xlsx") %>%
  mutate(date = as.Date(date),
         ppt_cm = pptmm/10) %>%
  mutate(gauge = "STR1E") %>%
  dplyr::select(date,gauge,ppt_cm) %>%
  complete(gauge,
           date = full_seq(as.Date(c("2011-01-01","2011-12-31")),1),
           fill = list(ppt_cm = 0))

#############
### 6. Combine and write to file
tbrg_2011_2013 %>%
  bind_rows(lter_2011_2013) %>%
  bind_rows(jer_2011_2013) %>%
  bind_rows(cdrrc_2011_2013) %>%
  bind_rows(stressorW_2011_2012) %>%
  bind_rows(stressorE_2011_2011) %>%
  write_csv("daily_climate/raw/daily_inputs_2011_2013.csv")
