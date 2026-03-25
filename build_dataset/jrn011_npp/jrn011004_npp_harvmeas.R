# jrn011004_npp_harvmeas.R
# formerly: build_dataset.210011004.R
# 

library(tidyverse)
library(readxl)

source('config.R')
out_path <- paste(im_path, 'Core_packages/210011004_npp_harvmeas', sep='/')

# Output data file name
f_out <- paste(out_path, "jrn011004_npp_ref_harvest_meas.test.csv", sep='/')

# Load the most recent datafile (should be a dated csv exported from the
# DataArchive folder), remove some columns that John has added, then export a 
# simplified dataframe as csv (without rownames)
# fname <- "source_data/nppharv_dataentry_1989-current_20230731.csv"
# df_in <- read_csv(fname) %>%
#   mutate(date = as.Date(date, '%m/%d/%Y')) %>%
#   select(-USDA_code, -Species_binomial, -form, -habit, -cpath)

# OR - just read straight from his spreadsheet
df_in = read_excel(paste(entry_path, 'Npp_Harv',
                   'nppharv_dataentry_1989-current.xlsx', sep='/'),
                   sheet='pq_for_IM_id-011004', na=c('NA', ''),
                   col_types = c('numeric', 'text', 'date', rep('text',3),
                                 rep('numeric',10), rep('text', 6))) %>%
  select(-USDA_code, -Species_binomial, -form, -habit, -cpath)

# Check for NAs and unique values of catvars
sapply(df_in, function(x) sum(is.na(x)))

df.export <- df_in
#df.export <- tm$merged %>%
#  select(everything(), comment = comment, -usda_plants_crossref_comment)

# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
# unique values
unique(df.export$year)
unique(df.export$season)
unique(df.export$zone)
unique(df.export$site)

# Export df.export as a csv to current directory
options(scipen=999)   # turns of scientific notation
write.csv(df.export, f_out, quote=F, row.names=F)

# Now the plantlist

# Output data file name
f_out <- paste(out_path, "jrn011004_plant_codes.csv", sep='/')

# Get the taxonomic merge code
## The chdir argument lets the sourced script use relative paths
source('R/taxa_code_merge.R', chdir=TRUE)
tm <- match_lter_codes(df_in, 'spp', im_path)

df.plantlist <- tm$plant_list %>%
  select(everything())

# Note that extra forms and cpaths are in the database for these (categories
# that may not appear in data)
unique(df.plantlist$form)
unique(df.plantlist$habit)
unique(df.plantlist$cpath)
unique(df.plantlist$lter_code) # 226
unique(df.plantlist$usda_code) # 165

# Export df.export as a csv to current directory
options(scipen=999)   # turns of scientific notation
write.csv(df.plantlist, f_out, quote=F, row.names=F)

