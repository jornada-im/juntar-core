
library(tidyverse)

read_csv("daily_climate/daily_estimate_ppt_gauge_priorities.csv") %>%
  left_join(read_csv("daily_climate/daily_estimate_ppt_gauge_characteristics.csv")) %>%
  write_csv("daily_climate/daily_estimate_ppt_gauge_usage.csv")
