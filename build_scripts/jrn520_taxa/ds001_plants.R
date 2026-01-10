# ds001_plants.R
# formerly build_dataset.210520001.R
# 
# BOILERPLATE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This is a template build script using R to prepare a dataset
# for EDI. You can safely remove this and other boilerplate
# and use the rest to design a new R script for your data.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# !!!!!!!!!
# When running this script on a nework drive it may be necessary to set the 
# path based on how you map the drive. i.e.:
#
# setwd('/Volumes/unix/path/to/datasets/210.../')
# setwd('Z:\\windows\path\to\datasets\210...\
#
# In rstudio, set the working directory to a path containing the script with:
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#
# !!!!!!!!!



# Set the working directory to a local or network share path
# (this only works in RStudio). 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(readxl)

# Path to incoming source data files
dsource <- "./source_data/"

# Output data file 1 name
f_out <- "jrn520001_JornadaPlantList.csv"

# Read John's plant list file
# The original format (Plntalfa._20220303.txt) is pretty tough to parse so it
# has been converted to an Excel sheet
f_in1 <- "JRN_plant_species_list_main.xlsx"

# Load data.
df_in <- read_xlsx(f_in1, skip=4, na=c('.', '', ' ', 'NA'))
names(df_in) <- tolower(names(df_in))

# Add a binomial name column
df <- df_in %>% mutate(
  bin_usda = paste(genus_usda, species_usda, sep=' '),
  bin_lter = paste(genus_lter, species_lter, sep=' ')
)
sapply(df, function(x) sum(is.na(x)))

library(taxadb)
df['itis_id'] <- get_ids(df$bin_usda)

df[is.na(df$itis_id),]
# Echinochloa crus-galli doesn't resolve to ITIS - should be ITIS:502210
# Might be able to use get_names here - there are synonyms...
df[df$bin_usda=='Echinochloa crus-galli', 'itis_id'] <- 'ITIS:502210'

# Check some correspondence between 
sum(df[df$bin_lter != 'NA NA','bin_usda']==df[df$bin_lter != 'NA NA','bin_lter'])
# Why is this different? Maybe the 3 non-plants?
sum(df$bin_lter==df$bin_usda)

sum(df$bin_lter==df$bin_usda)
df.export <- df_in 

# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
unique(df.export$date)
unique(df.export$plot)
unique(df.export$ppttrt)
unique(df.export$Ntrt)

# Export df.export as a csv to current directory
options(scipen=999)   # turns of scientific notation
write.csv(df.export, f_out, quote=F, row.names=F)