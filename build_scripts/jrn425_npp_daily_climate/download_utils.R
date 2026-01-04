### Downloading online data for daily precipitation and temperature from 15 NPP sites

library(curl)
library(tidyverse)
library(lubridate)

### Data from wireless sensors can be downloaded from EDI

# Package ID: knb-lter-jrn.210437046.7 Cataloging System:https://pasta.edirepository.org.
# Data set title: Jornada Basin LTER: Wireless meteorological station at NPP C-CALI site: Daily summary data:  2013 -          ongoing.
# Data set creator:  John Anderson - JRN LTER 
# Contact:  Data Manager - JRN-LTER Information Manager   - datamanager.jrn.lter@gmail.com
# Stylesheet v1.3 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#

getLatestVersion <- function(pkg){
  url_prefix <- "http://pasta.lternet.edu/package/eml/knb-lter-jrn/"
  filter_suffix <- "?filter=newest"
  
  this_url <- paste0(url_prefix,pkg,filter_suffix)
  
  this_version <- suppressWarnings(readLines(curl(this_url)))
  
  return(this_version)
}

pkgs <- read_csv("../NPP-data/daily_climate/siteLookup.csv") %>%
  mutate(version = purrr::map(pkg,getLatestVersion)) %>%
  unnest(version)

pkgs %>%  # keep record of versions used
  write_csv("../NPP-data/daily_climate/siteLookup.csv")


getSiteData <- function(SITE){
  site_data <- pkgs %>%
    filter(site == SITE)
  
  file_prefix <- paste("http://pasta.lternet.edu/package/data/eml/knb-lter-jrn",
                       site_data$pkg,
                       site_data$version, 
                       sep = "/")
  
  entity <- read.table(file_prefix)$V1
  
  site_file <- paste("http://pasta.lternet.edu/package/data/eml/knb-lter-jrn",
                     site_data$pkg,
                     site_data$version, 
                     entity,
                     sep = "/")
  
   #formatting from 'tidyr' example on EDI
   all_data <- read_csv(site_file,
                    skip=3, 
                    na=c( " ",".","NA"))
   
   # Return just ppt and temp
   all_data %>%
     dplyr::select(Date,Ppt_mm_Tot,Air_TempC_Min,Air_TempC_Max,
                   Flag_Ppt_mm_Tot,Flag_Air_TempC_Min,Flag_Air_TempC_Max) %>%
     mutate(site = SITE) %>%
     rename(date = Date)
  
    
}

getAllSites <- function(){
  sites <- pkgs$site
  
  all_sites <- tibble()
  for(site in sites){
    
    all_sites <- all_sites %>%
      bind_rows(getSiteData(site))
  }
  
  return(all_sites)
    
}

