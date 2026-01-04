library(tidyverse)

all_rg <- read_csv("daily_climate/nearby_gauge_coord.csv") 

dist_mat <- as.matrix(dist(all_rg[,c(2,3)]))

colnames(dist_mat) <- all_rg$gauge
rownames(dist_mat) <- all_rg$gauge

npp_dist <- dist_mat %>%
  as_tibble(rownames = "gauge") %>%
  gather(gauge2, distance, -gauge) %>%
  left_join(all_rg %>%
              dplyr::select(gauge,type)) %>%
  left_join(all_rg %>%
              dplyr::select(gauge,type),
            by = c("gauge2" = "gauge")) %>%
  filter(grepl("NPP",type.x)) %>%
  mutate(affilitation = ifelse(grepl("NPP|BIO|LTER",type.y), 
                                     "LTER",
                                     ifelse(grepl("USDA",type.y), 
                                                  "USDA JER",
                                                  "CDRRC"
                                            )
                               )
         ) %>%
  dplyr::select(-contains("type")) %>%
  arrange(gauge,distance)




#Assign distances to borrowing rules
read_csv("daily_climate/nearby_daily_gauges.csv") %>%
  left_join(npp_dist, by = c("site" = "gauge",
                             "gauge" = "gauge2")) %>%
  mutate(distance = round(distance)) %>%
  distinct() %>%
  write_csv("daily_climate/nearby_daily_gauges_dist.csv")
  




