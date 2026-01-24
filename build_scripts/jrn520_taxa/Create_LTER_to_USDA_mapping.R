
# Create a species list for translating LTER codes into USDA codes
library(tidyverse)

# Import JRN plant species list
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210520001/1/0614c0c3ac8a6f7c7ae047656be65a8e") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt1 
veglist.edi.import <- read_delim(infile1  
                                 ,delim=","   
                                 ,skip=1 
                                 , col_names=c( 
                                   "Family",   
                                   "Genus_USDA",   
                                   "Species_USDA",   
                                   "further_rank_USDA",   
                                   "alias",   
                                   "Reproduction",   
                                   "LTER_core",   
                                   "USDA_code",   
                                   "LTER_code",   
                                   "Genus_LTER",   
                                   "Species_LTER",   
                                   "Habit",   
                                   "Form",   
                                   "Habitat",   
                                   "Phenology",   
                                   "Pathway",   
                                   "Nativity",   
                                   "Citation_LTER"   ), 
                                 col_types=list( 
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character(),  
                                   col_character()), 
                                 na=c( " ",".","NA"))

LTER.to.USDA.dups <- veglist.edi.import %>%
  dplyr::filter(!is.na(LTER_code)) %>%
  group_by(LTER_code) %>%
  summarise(total = n()) %>%
  dplyr::filter(total > 1)

dups.detail <- LTER.to.USDA.dups %>%
  left_join(veglist.edi.import) %>%
  dplyr::select(LTER_code, USDA_code, total, Genus_USDA, Species_USDA, further_rank_USDA, Genus_LTER, Species_LTER)



# LTER codes that correspond to more than one USDA code
LTER.to.USDA.dups.distinct <- veglist.edi.import %>%
  dplyr::filter(!is.na(LTER_code)) %>%
  distinct(LTER_code, USDA_code) %>%
  group_by(LTER_code) %>%
  summarise(total = n()) %>%
  dplyr::filter(total > 1) %>%
  left_join(veglist.edi.import) %>%
  dplyr::select(LTER_code, USDA_code, total, Genus_USDA, Species_USDA, further_rank_USDA, Genus_LTER, Species_LTER) %>%
  mutate(type = "Multiple LTER codes for the same USDA code")

# USDA codes that correspond to more than one LTER code
USDA.to.LTER.dups.distinct <- veglist.edi.import %>%
  dplyr::filter(!is.na(USDA_code)) %>%
  distinct(LTER_code, USDA_code) %>%
  group_by(USDA_code) %>%
  summarise(total = n()) %>%
  dplyr::filter(total > 1) %>%
  left_join(veglist.edi.import) %>%
  dplyr::select(LTER_code, USDA_code, total, Genus_USDA, Species_USDA, further_rank_USDA, Genus_LTER, Species_LTER) %>%
  mutate(type = "Multiple USDA codes for the same LTER code")

mult.codes.stack <- LTER.to.USDA.dups.distinct %>%
  bind_rows(USDA.to.LTER.dups.distinct)

write_csv(mult.codes.stack, "D:/PASTA/Vegetation Species codes/One-to-many USDA-LTER plant codes.csv", na = "")


veglist.edi.import %>%
  dplyr::filter(!is.na(USDA_code)) %>%
  group_by(USDA_code) %>%
  summarise(total = n()) %>%
  dplyr::filter(total > 1)

# John.reconcile <- veglist.edi.import %>%
#   dplyr::filter(LTER_code %in% LTER.to.USDA.dups$LTER_code)
# 
# write.csv(John.reconcile, "//jornada-netb1/DataProducts/LTER_IM/TaxonomicCoverage/codes to reconcile.csv", row.names = FALSE, na = ".")

check <- veglist.edi.import %>%
  #dplyr::filter(USDA_code == "ECTR"| USDA_code == "MALE2") 
  dplyr::filter(LTER_code == "ECTR"| LTER_code == "SILE") 

species.list.LTER.to.USDA <- veglist.edi.import %>%
  dplyr::filter(USDA_code != "ECTR" & USDA_code != "MALE2") %>%
  dplyr::filter(!is.na(LTER_code)) %>%
  mutate(Species_binomial = paste(Genus_USDA, Species_USDA, sep = " ")) %>%
  dplyr::select(LTER_code, USDA_code, Species_binomial) %>%
  distinct()

species.list.LTER.to.USDA %>%
  group_by(LTER_code) %>%
  summarise(total = n()) %>%
  dplyr::filter(total > 1)

species.list.LTER.to.USDA %>%
  group_by(USDA_code) %>%
  summarise(total = n()) %>%
  dplyr::filter(total > 1)

unique(species.list.LTER.to.USDA$LTER_code)

write.csv(species.list.LTER.to.USDA, "//jornada-netb1/DataProducts/LTER_IM/TaxonomicCoverage/LTER_to_USDA_PLANTS_codes.csv", row.names = FALSE)
