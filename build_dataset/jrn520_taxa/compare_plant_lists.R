library(tidyverse)
library(readxl)
source("./config.R")

# Path to the plant list files (jrn520)
plants_path <- paste(im_path, "dataprep", "jrn520_taxa", "plants", sep="/")

## Primary Jornada plant list
mainlist <- read_excel(paste(plants_path, "jornada_plant_list_MAIN.xlsx", sep="/"),
                     skip=4, na = c(".", "NA")) |>
  rename_with(tolower) |>
  rename(cpath=pathway) |>
  mutate(spbinomial = paste(genus_lter, species_lter, sep=" ")) # create sp binomial
main_present <- mainlist |> filter(lter_core=="present") # Filter for taxa observed at the Jornada
## John Anderson's list
johnslist <- read_excel(paste(plants_path, "0_test-merge1_20260123.xlsx", sep="/"),
                        skip=2, na = c(".", "NA")) |>
  rename_with(tolower) |>
  rename(lter_code=lter_spp, usda_code=`current usda code`, spbinomial=lter_full_species_name) |>
  filter(nchar(lter_code) < 5)
## Kelly Allred's Flora of the Jornada Plain list
allredlist <- read_csv(paste(plants_path, "allred_jornada_spp_table_claude_v3.csv", sep="/"),
                           na = c(".", "NA",""))


# The JRN LTER field codes list
fieldlist <- read_excel(paste(im_path, "dataprep", "jrn520_taxa", "fieldcodes",
                            "lter_field_codes_MAIN.xlsx", sep="/"), skip=2)

# The plant list/trait table from the NPP project
jrn011list <- read.csv(paste(im_path, "dataprep", "jrn011_npp", "anpp", "function_list.csv", sep="/"), 2,
                       stringsAsFactors = FALSE)


# Find members from John's list not in the main list
johns_not_in_main <- johnslist[!johnslist$LTER_spp %in% mainlist$LTER_code,]
# Find codes from John's list not in the main code crosswalk
johns_not_in_field <- johnslist[!johnslist$LTER_spp %in% fieldlist$field_code,]

sum(!johnslist$LTER_spp %in% main_present$LTER_code)
sum(!main_present$USDA_code %in% johnslist$`CURRENT USDA code`)
sum(!mainlist$LTER_code %in% allredv9$`LTER Code`)
sum(!mainlist$USDA_code %in% allredv9$`USDA Code`)

john_LTER_not_in_main <- johnslist[!johnslist$LTER_spp %in% mainlist$LTER_code,]
john_USDA_not_in_main <- johnslist[!johnslist$`CURRENT USDA code` %in% mainlist$USDA_code,]

# Compare all john and darren's and allred codes to the field code list
john_LTER_not_in_field <- johnslist[!johnslist$LTER_spp %in% fieldlist$field_code,]
main_LTER_not_in_field <- mainlist[!mainlist$LTER_code %in% fieldlist$field_code,]
allred_LTER_not_in_field <- allredlist[!allredlist$`LTER Code` %in% fieldlist$field_code,]

# Still some issues with this
crosswalk <- mainlist[c('lter_code', 'spbinomial', 'usda_code', 'habit', 'form', 'cpath')] %>%
  full_join(johnslist[c('lter_code', 'spbinomial', 'usda_code', 'habit', 'form', 'cpath')],
  by = "lter_code", suffix = c("_main","_john"), keep = TRUE) |>
  left_join(allredlist[c('lter_code', 'spbinomial', 'usda_code')], by = join_by("lter_code_main"=="lter_code"),
            keep=TRUE, suffix=c("_allred","_allred")) |>
  select(starts_with("lter_code"), starts_with("usda_code"), starts_with('spbinomial'),
  ends_with("main"), ends_with("john"))
  
