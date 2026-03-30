# ds001_detailed_biomass.R
# formerly: build_dataset.210011001.R
# 

library(tidyverse)

source('config.R')
in_path <- paste(im_path, "dataprep/jrn011_npp/biomass/processed", sep='/')
out_path <- paste(im_path, 'Core_packages/210011001_npp_detailed_biomass', sep='/')

# Output data file name
f_out <- paste(out_path, "jrn011001_NPP_quadrat_estimates_SppSiteSeas.test.csv", sep='/')

# Input data
f_in <- 'biomass_reference_test.csv'

# Read in data file from Heather that goes to 2017
df <- read_csv(paste(in_path, f_in, sep="/"),
                skip=0, na = c(".", "","NA")) %>%
  select(-form, -path)

# Get the taxonomic merge code
## The chdir argument lets the sourced script use relative paths
source('R/taxa_code_merge.R', chdir=TRUE)
tm <- match_lter_codes(df, 'spp', im_path)

unmapped <- tm$unmapped_codes
# just MISS and NA

# Create final dataset - remove a couple columns from crossref
df.export <- tm$merged #%>%
  #filter(year < 2023)
# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
unique(df.export$year)
unique(df.export$season)
unique(df.export$zone)
unique(df.export$site)
unique(df.export$quad)
unique(df.export$form)
unique(df.export$cpath)
unique(df.export$habit)

# Export df.export as a csv to current directory
options(scipen=999)   # turns of scientific notation
write.csv(df.export, f_out, quote=F, row.names=F)

# Now the plantlist

# Output data file name
f_out <- paste(out_path, "jrn011001_plant_codes.csv", sep='/')

df.plantlist <- tm$plant_list %>%
  select(everything())

# Note that extra forms and cpaths are in the database for these (categories
# that may not appear in data)
unique(df.plantlist$form)
unique(df.plantlist$habit)
unique(df.plantlist$cpath)
unique(df.plantlist$spp) # 226
unique(df.plantlist$usda_code) # 165

# Export df.export as a csv to current directory
options(scipen=999)   # turns of scientific notation
write.csv(df.plantlist, f_out, quote=F, row.names=F)