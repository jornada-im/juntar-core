
library(tidyverse)

yrs <- 2014:2019

met <- tibble()
for(y in yrs){
  met <- met %>%
    bind_rows(read.table(paste0("https://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/",
                                y,
                                "/CRND0103-",
                                y,
                                "-NM_Las_Cruces_20_N.txt")))
}

met %>%
  mutate(date = as.Date(as.character(V2),"%Y%m%d"),
         Ta_max = ifelse(V10 == -9999, NA, V6),
         Ta_min = ifelse(V10 == -9999, NA, V7),
         ppt = ifelse(V10 == -9999, NA, V10)/10
  )  %>%
  dplyr::select(date:ppt) %>%
  write_csv("daily_climate/raw/hq_noaa_p_t_2014_2019.csv")
