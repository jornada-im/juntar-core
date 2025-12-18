# ds001_detailed_biomass.R
# formerly: build_dataset.210011001.R
# 

library(tidyverse)

source('config.R')
output.path <- paste(im.path, 'Core_packages/210011001_npp_detailed_biomass', sep='/')

# Output data file name
f_out <- paste(output.path, "jrn011001_NPP_quadrat_estimates_SppSiteSeas.test.csv", sep='/')

# Input data
f_in <- 'biomass_reference_test.csv'

# Read in data file from Heather that goes to 2017
df <- read_csv(paste0(dsource,f_in),
                skip=0, na = c(".", "","NA")) %>%
  select(-form, -path)

# Get the taxonomic merge code
## The chdir argument lets the sourced script use relative paths
source('../../TaxonomicCoverage/taxa_code_merge.R', chdir=TRUE)
tm <- merge_crossref_taxa(df, 'spp')

unmapped <- tm$unmapped_codes
# just MISS and NA

# Create final dataset - remove a couple columns from crossref
# Holding back 2023 data for now
df.export <- tm$merged %>%
  select(-Species_binomial, -usda_plants_crossref_comment) %>%
  filter(year < 2023)
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

