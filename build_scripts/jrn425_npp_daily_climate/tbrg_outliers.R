library(tidyverse)
library(lubridate)
library(ggthemes)

# tol_log <- -3
# tol <- eval(parse(text=paste0("1e",tol)))

#tol_log <- 0

## bring in the xscale met stations too
# xscale_ppt <- read.csv("../xscale/data/met_blocks_EDI.csv",
#                        stringsAsFactors = FALSE) %>%
#   as_tibble() %>%
#   mutate(ppt = ifelse(Flag_Ppt_mm_Tot == "A",as.numeric(Ppt_mm_Tot)/10,NA)) %>%
#   #filter(Flag_Ppt_mm_Tot == "A") %>%
#   mutate(site = paste0("xscale_",block)) %>%
#   mutate(date = ymd(Date)) %>%
#   dplyr::select(date,site,ppt) 

tbrg_dailies <- read_csv("daily_climate/raw/daily_climate_2013_present.csv") %>%
  mutate(ppt = ifelse(Flag_Ppt_mm_Tot == "A",as.numeric(Ppt_mm_Tot)/10,NA)) %>%
  #filter(Flag_Ppt_mm_Tot == "A") %>%
  #bind_rows(xscale_ppt) %>%
  mutate(ppt = log10(ppt),
         #ppt = ifelse(!is.finite(ppt), NA, ppt), # zeros are -Inf
         year = year(date),
         month = month(date)) %>%
  dplyr::select(date,site:month)
  



tbrg_dailies %>%
  ggplot(aes(as.factor(month),ppt)) +
  geom_boxplot() 

tbrg_dailies %>%
  ggplot(aes(month,ppt)) +
  geom_boxplot() 

partners <- 3
partner_cutoff <- 2

pairs <- tbrg_dailies %>%
  left_join(tbrg_dailies,  by = c("date","year","month")) %>%
  filter(site.x != site.y) %>%
  filter(site.x != "SMAL") %>% # because short record
  filter(complete.cases(.)) %>%  # excluding missing or invalid records 
  filter(is.finite(ppt.x) & is.finite(ppt.y)) %>% #excluding 0 ppt records
  mutate(rainy = month %in% 6:10) %>%
  group_by(site.x,site.y) %>%
  summarise(cor = cor(ppt.x,ppt.y)) %>%
  group_by(site.y) %>%
  arrange(-cor) %>%
  slice(1:partners) %>% 
  ungroup()


tbrg_single_outliers <- tbrg_dailies %>%
  left_join(tbrg_dailies,  by = c("date","year", "month")) %>%
  filter(site.x != site.y) %>%
  mutate(rainy = month %in% 6:10) %>%
  inner_join(pairs) %>%
  group_by(site.x,site.y) %>%
  nest()  %>%
  mutate(pred = purrr::map(data, 
                           function(df){
                             baseline <<- df %>% 
                               filter(is.finite(ppt.x) & is.finite(ppt.y)) 
                             recent <<- df %>% 
                               filter(is.finite(ppt.x))
                             lm(ppt.y ~ ppt.x, data=baseline) %>%
                               predict(newdata = recent,
                                       interval = "prediction",
                                       level = 0.95) %>%
                               as_tibble() %>%
                               bind_cols(recent)
                           })) %>%
  dplyr::select(-data) %>%
  unnest(pred) %>%
  mutate(outlier = (ppt.y < lwr | ppt.y > upr | is.na(ppt.y) | 10^ppt.y < 0.5*(10^ppt.x)),
         label = ifelse(outlier,
                        paste(site.y,"-",month.abb[month],year),
                        ""),
         season = ifelse(rainy,"Rainy Season", "Dormant"),
         ppt.x = 10^ppt.x,
         ppt.y = 10^ppt.y,
         lwr = 10^lwr,
         upr = 10^upr,
         fit = 10^fit) 



tbrg_outliers <- tbrg_single_outliers %>% 
  group_by(site.y,date) %>% 
  mutate(n_out = sum(outlier))

tbrg_outliers %>%
  dplyr::select(site.x:date,ppt.y:n_out) %>%
  mutate(side = ifelse(is.na(ppt.y),
                       "filled",
                              ifelse(ppt.y < lwr,
                                    "low",
                                    ifelse(ppt.y > upr,
                                           "high",
                                           "inside")))) %>%
  distinct(site.y,date,n_out,side) %>%
  filter(n_out >= partner_cutoff) %>%
  group_by(site.y,side) %>%
  count() %>%
  spread(side,n)

tbrg_outliers %>%
  distinct(site.y,date,n_out) %>%
  filter(n_out >= partner_cutoff) %>%
  group_by(date) %>%
  count() %>%
  arrange(-n) %>%
  ggplot(aes(as.factor(n))) +
  geom_histogram(stat="count") +
  xlab("# of locations (out of 15) marked as outliers") +
  ylab("distinct dates in mid 2013-present (N = 2,506)") +
  theme_few() +
  scale_y_continuous(expand = c(0,0))

tbrg_outliers %>%
  filter(site.y == "WELL" & year == 2016  & rainy) %>%
  ggplot(aes(ppt.x,ppt.y)) +
  geom_line(aes(y=fit)) + 
  geom_line(aes(y=lwr),linetype = 3) + 
  geom_line(aes(y=upr),linetype = 3) +
  geom_point(aes(color=ppt.y < lwr)) +
  facet_wrap(~site.x) +
  theme_few()

# tbrg_outliers  %>%
#   distinct(site.y,date,n_out,ppt.y) %>%
#   filter(year(date) == 2016 & month(date) %in% 6:8 & site.y == "WELL") %>%
#   ggplot(aes(date,ppt.y,fill=as.factor(n_out))) +
#   geom_col() +
#   facet_wrap(~site.y) +
#   theme_few()

# tbrg_outliers %>%
#   #filter(site.y == "BASN") %>%
#   ggplot(aes(ppt.x,ppt.y,color=as.factor(n_out),
#              text = paste(site.y,"-",month.abb[month],year))) +
#   # geom_line(aes(y = lwr,group = paste(site.x,site.y,rainy)),
#   #           size = 0.1,
#   #           color = "gray50") + 
#   # geom_line(aes(y = upr,group = paste(site.x,site.y,rainy)),
#   #           size = 0.1,
#   #           color = "gray50") + 
#   # geom_line(aes(y=fit,group = paste(site.x,site.y))) + 
#   #geom_abline() +
#   geom_point(size = 0.75) +
#   #ggrepel::geom_text_repel(aes(label = label)) + 
#   theme_few() +
#   # scale_y_log10() +
#   # scale_x_log10() +
#   xlab("Daily PPT at reference TBRG (cm)") +
#   ylab("Daily PPT at target TBRG (cm)") #+
#   # facet_wrap(~season,
#   #            nrow = 2) +
#   # theme(legend.position = "none")
# ggsave("daily_climate/figures/tbrg_daily_outliers.png",
#        type = "cairo",
#        width = 7.5,
#        height = 10)


replacements <- tbrg_outliers %>%
  filter(n_out >= partner_cutoff | is.na(ppt.y)) %>%  #I'm an outlier or I'm missing
  #filter(outlier) %>%   ## do this?
  #filter(!is.na(ppt.x)) %>%
  group_by(site.y,date) %>%
  summarise(replacement = weighted.mean(fit,cor)) %>%
  ungroup() %>%
  rename(site = site.y)

zeros <- tbrg_dailies %>%
  left_join(tbrg_dailies,  by = c("date","year", "month")) %>%
  filter(site.x != site.y) %>%
  mutate(rainy = month %in% 6:10) %>%
  inner_join(pairs) %>%
  mutate() %>%
  filter(!is.finite(ppt.y)) %>%
  filter(!is.na(ppt.x)) %>%
  group_by(date,site.y,ppt.y) %>%
  filter(sum(!is.finite(ppt.x)) == n()) %>%
  ungroup() %>%
  distinct(date,site.y,ppt.y) %>%
  rename(site = site.y) %>%
  mutate(ppt.y = recode(ppt.y, .missing = -Inf)) %>%
  mutate(ppt.y = 10^ppt.y)

## Make final daily df.
final_daily <- replacements %>%
  bind_rows(zeros %>%
              rename(replacement = ppt.y)) %>%
  full_join(tbrg_dailies %>%
              dplyr::select(date,site,ppt) %>%
              mutate(ppt = 10^ppt)) %>%
  #filter(!grepl("xscale",site)) %>%
  # mutate(replacement = (10^replacement),
  #        ppt = (10^ppt)) %>%
  mutate(ppt_cm = coalesce(replacement,ppt),
         replaced =  ifelse(ppt == ppt_cm | is.na(ppt),
                            "kept or filled",
                            ifelse(ppt > ppt_cm | (is.na(ppt) & ppt_cm > 0),
                                   "lowered",
                                   "raised"))) %>%
  dplyr::select(date,site,ppt_cm,ppt,replaced)

final_daily %>%
  filter(is.finite(ppt)) %>%
  # filter(site %in% c("WELL","RABB","NORT","IBPE") &
  #          year(date) == 2016) %>%
  ggplot(aes(date,ppt_cm,
             color = replaced)) +
  geom_point(aes(size = replaced)) +
  geom_segment(aes(y = ppt, yend = ppt_cm, xend = date),
               linetype = 3,
               size = 0.5) +
  facet_wrap(~site) +
  theme_few() +
  scale_color_manual(values = c("gray50","red","blue")) +
  scale_size_manual(values = c(0.1,0.5,0.5)) +
  theme(legend.position = c(0.1,0.9))

final_daily %>%
  group_by(replaced) %>%
  count() %>%
  ungroup %>%
  mutate(n = 100*n/sum(n))




final_daily %>%
  complete(date = full_seq(date,1),
           site) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  filter(month %in% 4:9 & year %in% 2016:2018) %>%
  group_by(site,year) %>%
  summarise(ppt_gs = sum(ppt_cm)) %>%
  filter(site %in% c("WELL","RABB","NORT","IBPE")) %>%
  ggplot(aes(year,ppt_gs)) +
  geom_col() + 
  facet_wrap(~site)


final_daily %>%
  dplyr::select(date,site,ppt_cm) %>%
  write_csv("daily_climate/processed/corrected_daily_tbrg.csv")

