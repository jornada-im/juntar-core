# ds003_summaries.R
# formerly: build_dataset.210011003.R
# 

library(tidyverse)

source('config.R')
output.path <- paste(im.path, 'Core_packages/210011003_npp_summaries', sep='/')

# Output data file name
f_out <- paste(output.path, "jrn011003_NPP_annual_site_summary.test.csv", sep='/')

# Input data
f_in <- "npp_annual_test.csv"
# Read in data file 
df <- read_csv(paste0(dsource,f_in),
               skip=0, na = c(".", "","NA")) %>%
  select(year, zone=ZONE, site=SITE, npp_g_m2 = `annual NPP (g/m2)`)


df.export <- df %>%
  filter(year < 2023)
# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
unique(df.export$year)
unique(df.export$zone)
unique(df.export$site)


# Export df.export as a csv to current directory
options(scipen=999)   # turns of scientific notation
write.csv(df.export, f_out, quote=F, row.names=F)

