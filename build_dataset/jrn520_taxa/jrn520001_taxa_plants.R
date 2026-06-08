# ds001_plants.R
# formerly build_dataset.210520001.R

library(tidyverse)
library(readxl)

source('config.R')
out_path <- file.path(im_path, "NonCore_packages", "210520001_taxa_plants")
# Path to the plant list files (jrn520)
in_path <- file.path(im_path, "dataprep", "jrn520_taxa", "plants")


# Output data file 1 name
f_out <- "jrn520001_JornadaPlantList.csv"

# Read the current plant list file. Was originally assembled by Darren, using
# John Anderson's list and one from Justin Van Zee.
# John's original format (Plntalfa._20220303.txt) was pretty tough to parse so it
# has been converted to an Excel sheet
df_in <- read_excel(file.path(plants_path, "jrn_plant_list_MAIN.xlsx"), sheet='plant_list',
                        skip=4, na = c(".", "NA"))

# Add a column for ITIS ids from taxadb
library(taxadb)
df_in['itis_id'] <- get_ids(df_in$sciname)

# About 25 taxa don't resolve at ITIS
df_in[is.na(df_in$itis_id),]

# Echinochloa crus-galli doesn't resolve to ITIS - should be ITIS:502210
# Might be able to use get_names here - there are synonyms...
# df[df$bin_usda=='Echinochloa crus-galli', 'itis_id'] <- 'ITIS:502210'

df.export <- df_in 

# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
unique(df.export$family)
unique(df.export$habit)
unique(df.export$form)
unique(df.export$cpath)
unique(df.export$nativity)
unique(df.export$habitat)
unique(df.export$phenology)
unique(df.export$reproduction)
unique(df.export$lter_observed)
unique(df.export$sciname_auth_follows)
unique(df.export$usda_code_is_syn)

# Export df.export as a csv
options(scipen=999)   # turns of scientific notation
write.csv(df.export, file.path(out_path, f_out), quote=F, row.names=F)

# Output data file 2 name
f_out <- "jrn520001_JornadaPlantList_synonyms.csv"

# Read the synonyms file
df_in <- read_excel(file.path(plants_path, "jrn_plant_list_MAIN.xlsx"), sheet='plant_synonyms',
                        skip=4, na = c(".", "NA"))

# 
df.export <- df_in 

# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
#unique(df.export$...)


# Export df.export as a csv to current directory
options(scipen=999)   # turns of scientific notation
write.csv(df.export, file.path(out_path, f_out), quote=F, row.names=F)
