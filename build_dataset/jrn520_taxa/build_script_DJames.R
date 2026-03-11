
library(tidyverse)
library(readxl)

# Set working directory to location where script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir()

# Paths to metadata templates, data entities, and eml
datafiles <- c("source_data/JRN_vascular_plant_species_list_DJames.csv")      # Data entity file names
sourcepath <- "./source_data/"                # Path to source data

# Import manually edited file of LTER codes
#
# This is essentially John Anderson's list - it was reformatted from the
# Plntalfa.docx file (via the text file).
# Note that John follows the Corell and Johnston taxonomy
#
# According to John Anderson: leave RHBE off the list
LTER.plants.edit <- read_xlsx(paste0(sourcepath, "Plntalfa_table_edit.xlsx"),
                              sheet = 'LTER_plant_list_editing', skip = 3) %>%
  filter(LTER.code != "RHBE") %>%
  mutate(Habit = if_else(Habit == ".", as.character(NA), Habit),
         Habitat = if_else(Habitat == ".", as.character(NA), Habitat),
         Phenology = if_else(Phenology == ".", as.character(NA), Phenology),
         Pathway = if_else(Pathway == ".", as.character(NA), Pathway),
         Stat = if_else(Stat == ".", as.character(NA), Stat),
         Form = if_else(Form == "FORM", "FORB", Form),
         Habitat = if_else(Habitat == "GB TW", "GB,TW",
                           if_else(Habitat == "TWTE", "TW,TE",
                                   if_else(Habitat == "PTTE", "PT,TE",
                                           if_else(Habitat == "UP13", ".", Habitat)))))

unique(LTER.plants.edit$Habit)
unique(LTER.plants.edit$Habitat)
unique(LTER.plants.edit$Phenology)
unique(LTER.plants.edit$Pathway)
unique(LTER.plants.edit$Stat)

######################################
# Import JER seed plant list with USDA codes

# These are Justin's lists using the Kelly Allred Jornada taxonomy.
JER.plants0 <- read_xlsx(paste0(sourcepath, "plantListJER.xlsx"), sheet = 'Seed plants') %>%
  dplyr::select(-pg, -`C&J_name`, -Code, -`Alternative Common Name`, -`No info available`, -`Questionable info`) %>%
  rename(Common.name = `Common name(s)`,
         Old.names = `Old name(s)`,
         LTER.code = `LTER Code`) %>%
  mutate(seed_spore = "seed")
# Import spore plant list
JER.spores <- read_xlsx(paste0(sourcepath, "plantListJER.xlsx"), sheet = 'Spore plants') %>%
  dplyr::select(-Division) %>%
  mutate(seed_spore = "spore")

# Stack seed and spore plants together
JER.plants <- bind_rows(JER.plants0, JER.spores) %>%
  rename(USDA.code = `USDA code`,
         further_rank = Other)

# Check for missing USDA codes
JER.plants %>%
  filter(is.na(USDA.code))

# Check for missing LTER codes in seed plants
# There are missing LTER codes for spore plants
JER.plants %>%
  filter(seed_spore == "seed" & is.na(LTER.code))

# Find any missing LTER codes in JER.plants
## First get an LTER code dataframe
JER.plants.LTER.codes <- JER.plants %>%
  dplyr::select(LTER.code) %>%
  distinct() %>%
  mutate(USDA = "present")

## Merge unique LTER codes in USDA list with LTER codes in LTER plant list
## Note that the LTER plant list is already unique
LTER.codes.merge <- merge(data.frame(LTER.code = LTER.plants.edit$LTER.code, LTER = "present", stringsAsFactors = FALSE), JER.plants.LTER.codes, by = "LTER.code", all.x = TRUE) 

## Identify the codes that do not match up
LTER.codes.merge %>% filter(is.na(USDA) | is.na (LTER))

# Perform a preliminary merge of the LTER and USDA lists to identify things that need to be reconciled
# Prepare LTER list for merging
LTER.merge.prepare <- LTER.plants.edit %>%
  dplyr::select(LTER.code, Genus, species, Family) %>%
  rename(Genus.LTER = Genus,
         species.LTER = species,
         Family.LTER = Family)

# Do initial merge
plants.merge <- merge(JER.plants, LTER.merge.prepare, by = "LTER.code", all = TRUE)

# Check to see if there are any non-unique LTER codes
LTER.codes.check <- plants.merge %>%
  dplyr::select(LTER.code, Family, Genus, Species) %>%
  distinct() %>%
  group_by(LTER.code) %>%
  summarise(total = n()) %>%
  filter(total > 1)

check.LTER <- plants.merge %>%
  filter(LTER.code %in% LTER.codes.check$LTER.code) %>% 
  arrange(LTER.code) 
# There are 2 cases of each of LTER codes ECTR and SILE

# Check to see if there are any non-unique USDA codes
USDA.codes.check <- plants.merge %>%
  dplyr::select(USDA.code, Family, Genus, Species) %>%
  distinct() %>%
  group_by(USDA.code) %>%
  summarise(total = n()) %>%
  filter(total > 1)

check.USDA <- plants.merge %>%
  filter(USDA.code %in% USDA.codes.check$USDA.code) %>% 
  arrange(LTER.code)
# There are 2 cases of each of USDA codes PHCO and POOL

# Create a list of genus and/or species naming differences
merge.differences <- plants.merge  %>%
  dplyr::select(USDA.code, LTER.code, Family, Family.LTER, Genus, Genus.LTER, Species, species.LTER) %>%
  filter(Family != Family.LTER |
           Genus != Genus.LTER |
           Species != species.LTER) %>%
  mutate(Family_diff = if_else(Family == Family.LTER, "", "Family"),
         Genus_diff = if_else(Genus == Genus.LTER, "", "Genus"),
         Species_diff = if_else(Species == species.LTER, "", "species")) %>%
  # After reconciling family names
  dplyr::select(Family, Family.LTER, USDA.code, LTER.code, Genus,  Species, Genus.LTER, species.LTER, Genus_diff, Species_diff) %>%
  arrange(Species_diff, Genus_diff)

# Export differences list as a CSV
# write.csv(merge.differences, "Code differences to reconcile.csv", na = "", row.names = FALSE)

# Create species list for posting on EDI
USDA.merge.prepare2 <- JER.plants %>%
  dplyr::select(Family, Genus, Species, further_rank, LTER.code, USDA.code, Common.name, Old.names, seed_spore) %>%
  rename(common_name_USDA = Common.name) %>%
  distinct() #%>% filter(LTER.code %in% LTER.plants.edit$LTER.code)

LTER.merge.prepare2 <- LTER.plants.edit %>%
  rename(Genus_LTER = Genus,
         species_LTER = species,
         Family_LTER = Family,
         common_name_LTER = Common_name,
         alias_LTER = alias) %>%
  mutate(Citation_LTER = if_else(LTER.code %in% c("SOIL", "NONE", "ROAD"), paste(LTER.code), paste(Genus_LTER, species_LTER, citation_ext, sep = " ")),
         LTER_core = "present") %>%
  dplyr::select(-status, -citation_ext, -Names_text_string, -Reference, -Voucher)

LTER.USDA.merge <- merge(LTER.merge.prepare2, USDA.merge.prepare2, by = "LTER.code", all = TRUE)

# Check for different family names
LTER.USDA.merge %>% filter(!is.na(Genus_LTER) & Family != Family_LTER)
# CHeck for species whose codes appear twice
LTER.USDA.merge %>% filter(USDA.code %in% c("PHCO", "POOL") | LTER.code %in% c("ECTR", "SILE"))

unique(LTER.USDA.merge$Stat)
  
# Edit the species list for posting
plant.list.edi <- LTER.USDA.merge %>%
  mutate(common_name = if_else(is.na(common_name_LTER), common_name_LTER, common_name_USDA),
         alias = if_else(!is.na(alias_LTER), alias_LTER,
                         if_else(!is.na(Old.names), Old.names, as.character(NA))),
         LTER_core = if_else(is.na(LTER_core),"not observed", LTER_core),
         Nativity = if_else(Stat == "NAT", "native",
                            if_else(Stat == "INT", "introduced", as.character(NA)))) %>%
  rename(LTER_code = LTER.code,
         USDA_code = USDA.code,
         Species_LTER = species_LTER,
         Genus_USDA = Genus,
         Species_USDA = Species,
         further_rank_USDA = further_rank,
         Reproduction = seed_spore) %>%
  dplyr::select(Family, Genus_USDA, Species_USDA, further_rank_USDA, alias, Reproduction, LTER_core, USDA_code, LTER_code, Genus_LTER, Species_LTER, 
                Habit, Form, Habitat, Phenology, Pathway, Nativity, Citation_LTER)

plant.list.edi$Habitat <- sub(pattern = ",", replacement = ";", x = plant.list.edi$Habitat)
  

unique(plant.list.edi$Habit)
unique(plant.list.edi$Habitat)
unique(plant.list.edi$Phenology)
unique(plant.list.edi$Pathway)
unique(plant.list.edi$Stat)

# Export df.export as a csv to current directory (no rownames or quoting)
options(scipen=999)   # turns of scientific notation
write.csv(plant.list.edi, datafiles[1], na = ".", row.names = FALSE,
          quote = F)  

# Create some tables to use in metadata
# List of USDA codes appearing multiple times
USDA.codes.multi <- plant.list.edi %>%
  group_by(USDA_code) %>%
  summarise(total = n()) %>%
  filter(total > 1)
# write.csv(USDA.codes.multi, "USDA codes appearing multiple times.csv", row.names = FALSE)

# List of LTER codes appearing multiple times
LTER.codes.multi <- plant.list.edi %>%
  group_by(LTER_code) %>%
  summarise(total = n()) %>%
  filter(total > 1)
#write.csv(LTER.codes.multi, "LTER codes appearing multiple times.csv", row.names = FALSE)

# List of taxonomic differences
taxon.diffs <- plant.list.edi %>% 
  filter(LTER_code %in% c("ECTR", "SILE") | USDA_code %in% c("GLBI2", "PHCO", "POOL"))
# write.csv(taxon.diffs, "Taxonomic differences between LTER and USDA codes.csv", row.names = FALSE)
