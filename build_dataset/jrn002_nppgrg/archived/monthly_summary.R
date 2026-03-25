### This file reads in the raw ppt data, assigns it 'real' months, then finds the monthly sums. 
### This is done in order to make the next step of seasonal or annual summaries easier. 
## Heather Savoy (hsavoy.jrn.lter@gmail.com)
## Started 02/27/2019

library(tidyverse)
library(purrr)
library(lubridate)

## the working directory is assumed to be the NPP-data repo

## 1. Read in the raw ppt data. The chosen unit of measurement is cm.
raw_data <- read_csv("ppt/raw/GRG-network-data.csv",
                     skip = 2) %>%
  mutate(date = mdy(date),
         p_cm = as.numeric(ppt_mm)/10) %>%
  filter(!is.na(date) & !(site %in% c("P12A","NFLM","SFLM","UPTR","BIOD","SMLM")) ) 

## 2. Assign 'real' month. This is done since the grg collection are only 
## somewhat regular and don't line up with the months. Since subsequent 
## temporal aggregations will rely on monthly assigments, we first assign
## the most likely month assignment. For example, if a grg is collected
## Aug 20 and then Sept 3, then the second measurement would be assigned
## as Aug since the majority of the collection period was in Aug, not Sept. 

# A function to assign months to a series of dates
getMonth <- function(df){  
  diffs <- diff(df$date)
  months <- c()
  
  # I currently don't know the installation date, 
  # so I'm assuming the current month for the first record
  months[1] <- month(df$date[1])
  
  # For each pair of collections, find the most common month between them, 
  # then assign that month to the second collection as the 'real' month.
  for(i in 2:length(df$date)){          
    months[i] <- as.numeric(names(sort(table(month(seq(df$date[i]-diffs[i-1]+1,
                                                        df$date[i],1)
                                                   )
                                             ),
                                       decreasing = TRUE)[1]))
  }
  # Return 'real' months
  return(months)
}

# Apply the getMonth function to each site
real_months <- raw_data %>%
  group_by(site) %>%
  arrange(date) %>%
  nest() %>%
  mutate(month = purrr::map(data,getMonth)) %>%
  unnest(c(data,month))

## 3. For each month and site, sum all collections in order to estimate 
## monthly precipitation. 
monthly_sum <- real_months %>%
  mutate(year1 = year(date),
         year = ifelse(month(date) == 1 & month == 12, year1-1, year1)) %>%
  group_by(site,year,month) %>%
  summarise(p_cm = sum(p_cm, na.rm = TRUE)) %>%
  ungroup 


## 4. Store the monthly summaries as a .csv in the 'processed' folder. 
write.csv(monthly_sum,
          file = "ppt/processed/PPT_monthly.csv",
          quote = FALSE,
          row.names = FALSE)
