# ds001_daily_est_precip.R
#
# originally from build_dataset.210425001.R
# 
# BOILERPLATE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This is a template build script using R to prepare a dataset
# for EDI. You can safely remove this and other boilerplate
# and use the rest to design a new R script for your data.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# Set the working directory to a local or network share path
# (this only works in RStudio). 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# If this fails try something like these:
# setwd('/Volumes/unix/path/to/datasets/210.../')
# setwd('Z:\\windows\path\to\datasets\210...\)

library(tidyverse)

# Path to incoming source data files
#dsource <- "./source_data/"
dsource <- "./submissions/JRN_daily_gapfilling/data/"

# Output data file name
f_out <- "JRN425001_npp_estimated_daily_precip.csv"

p_in <- read_csv(paste0(dsource, "gapfilled_daily_ppt_1980_2020_flagged.csv"), 
                 skip = 0, na = c('NA', '.', '-9999')) %>%
  mutate(flag = replace(flag, is.na(flag), 'o'))

sapply(p_in, function(x) sum(is.na(x)))
unique(p_in$flag)


df.export <- p_in %>% mutate(
  date = as.Date(date)
)

# Export df.export as a csv to current directory (no rownames or quoting)
options(scipen=999)   # turns of scientific notation
write.csv(df.export, f_out, quote=F, row.names=F)

# Output data file name 2
dsource <- "./source_data/"
f_out2 <- "daily_gapfill_ppt_gauge_usage.csv"

# Now the rain gauges file
gf_in <- read_csv(paste0(dsource, "daily_gapfill_ppt_gauge_usage.csv"), 
                  skip = 0, na = c('NA', '.', '-9999')) %>%
  mutate(affiliation = recode(affiliation, "LTER" = "JRN_LTER",
                              "USDA JER" = "USDA_JER"),
         type = recode(type, weighing='wbrg', TBRG='tbrg',standard='dsrg'))

sapply(gf_in, function(x) sum(is.na(x)))
unique(gf_in$site)
unique(gf_in$priority)
unique(gf_in$gauge)
unique(gf_in$affiliation)
unique(gf_in$type)

df2.export <- gf_in %>% mutate(
  start = as.Date(start, '%m/%d/%y'),
  end = as.Date(end, '%m/%d/%y')
)

# Export df.export as a csv to current directory (no rownames or quoting)
options(scipen=999)   # turns of scientific notation
write.csv(df2.export, f_out2, quote=F, row.names=F)

# Output data file name 3
dsource <- "./submissions/JRN_daily_gapfilling/data/"
f_out3 <- "site_edi_pkg_versions.csv"

# Now the EDI package file
gf_in <- read_csv(paste0(dsource, "site_edi_pkg_versions.csv"), 
                  skip = 0, na = c('NA', '.', '-9999')) %>%
  mutate(Zone = recode(Zone, "Creosote" = "C","Grassland" = "G",
                              "Mesquite" = "M","Tarbush" = "T",
                              "Playa" = "P"))

sapply(gf_in, function(x) sum(is.na(x)))
unique(gf_in$JRN_ID)
unique(gf_in$Zone)
unique(gf_in$JRN_Num)
unique(gf_in$site)

df3.export <- gf_in

# Export df.export as a csv to current directory (no rownames or quoting)
options(scipen=999)   # turns of scientific notation
write.csv(df3.export, f_out3, quote=F, row.names=F)

# Move the methods file
file.copy(paste0("./submissions/JRN_daily_gapfilling/doc/", 'Method_gapfill_daily_precipitation_20230301.pdf'),
          './Method_gapfill_daily_precipitation_20230301.pdf')
