#### Comparing wireless data to grg data

wireless_2013_2019 <- read_csv("daily_climate/raw/daily_climate_2013_present.csv") %>%
  filter(is.na(Flag_Ppt_mm_Tot)) %>%
  dplyr::select(date,site,Ppt_mm_Tot) 

grg_1989_2019 <- read_csv("ppt/raw/GRG-network-data.csv",
                 skip = 2) %>%
  mutate(date = mdy(date))  %>%
  filter(!(site %in% c("P12A","NFLM","SFLM","UPTR","BIOD","SMLM"))) %>%  
  mutate(p_mm = as.numeric(ppt_mm)) %>%
  dplyr::select(date,site,p_mm)


grg2 <- grg_1989_2019 %>%
  filter(date >= "2013-01-01") %>%
  arrange(site,date) %>%
  rowid_to_column() %>%
  group_by(site) %>%
  mutate(rowid = rowid - min(rowid))

getSum <- function(df,wireless = wireless_2013_2019){
  SITE <- unique(df$site)
  dates <- full_seq(c(df$date.x+1,df$date.y),1)
  
  ws <- wireless %>%
    filter(site == SITE) %>%
    filter(date %in% dates) %>%
    mutate(sum = sum(Ppt_mm_Tot, na.rm = TRUE))
  
  return(unique(ws$sum))
}

both <- grg2 %>%
  left_join(grg2 %>%
              mutate(rowid=rowid-1), 
            by = c("rowid","site")) %>%
  filter(!is.na(date.y)) %>%
  mutate(id = paste0(site,rowid)) %>%
  group_by(id) %>%
  nest() %>%
  mutate(wsum = purrr::map(data, getSum)) %>%
  unnest(c(data,wsum))

both %>%
  filter(!is.na(wsum)) %>%
  ggplot(aes(p_mm.y, wsum)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

both %>%
  filter(date.y > "2016-01-01") %>%
  ungroup %>%
  rename(wireless = wsum,
         grg = p_mm.y) %>%
  dplyr::select(site,date.y,grg,wireless) %>%
  gather(type,ppt,-c(site,date.y)) %>%
  filter(!is.na(ppt)) %>%
  mutate(year = format(date.y,"%Y")) %>%
  group_by(site,type,year) %>%
  mutate(ppt = cumsum(ppt)) %>%
  ggplot(aes(date.y,ppt,color=type)) +
  geom_step() +
  facet_wrap(~site) +
  theme_bw() +
  theme(legend.position = c(0.9,0.1)) 
ggsave("daily_climate/figures/ppt_comparison.png",
       width = 4,
       height = 4)

