# build_dataset.210262008.R

dsid <- '210262008'
source('config.R')
dirname <- dir(core.path)[grepl(dsid, dir(core.path))]
source.path <- paste(core.path, dirname, "source_data", sep='/')
out.path <- paste(core.path, dirname, sep='/')

#setwd(path)


library(tidyverse)

# Import the data----
first.capture.import0 <- read_csv(paste0(source.path, "/", "Ecotone_Rodent_Capture_UPDATE.csv"),
                                  na = c(".", "")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

with(first.capture.import0, table(Site, Year, Habitat))

# In 2019, Grassland and SHrubland are missing from JER Pasture 12A
# Bob sent another data set to append
append2019 <- read_csv(paste0(source.path, "/", "Ecotone_Rodent_Capture_2019_TO_ADD.csv"),
                       na = c(".", "")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))


# Now add recent data 2022-onward
first.capture.2022 <- readxl::read_excel(paste0(source.path, "/", "EcotoneRodent_2022.xlsx"),
                                         sheet = 'Allcaps', na = c('NA','.')) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Site = as.character(Site),
         Site = case_match(Site, "3"~"CDRRC Pasture 3", "9"~"JER Pasture 9", "12"~"JER Pasture 12A")) %>%
  rename(Tag='Tag #', Station='Station #', Species_code="Species") %>%
  select(Year,Date,Night,Site,Habitat,Station,Tag,Species_code,Sex,Rep,Status,Weight)

first.capture.2023 <- readxl::read_excel(paste0(source.path, "/", "/EcotoneRodent_2023.xlsx"),
                                         sheet = 'Allcaps', na = c('NA','.')) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Site = as.character(Site),
         Site = case_match(Site, "3"~"CDRRC Pasture 3", "9"~"JER Pasture 9", "12"~"JER Pasture 12A")) %>%
  rename(Tag='Tag #', Station='Station #', Species_code="Species") %>%
  select(Year,Date,Night,Site,Habitat,Station,Tag,Species_code,Sex,Rep,Status,Weight)

first.capture.2024 <- readxl::read_excel(paste0(source.path, "/", "EcotoneRodent_2024.xlsx"),
                                         sheet = 'Allcaps', na = c('NA','.')) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Site = as.character(Site),
         Site = case_match(Site, "3"~"CDRRC Pasture 3", "9"~"JER Pasture 9", "12"~"JER Pasture 12A")) %>%
  rename(Tag='Tag #', Station='Station #', Species_code="Species") %>%
  select(Year,Date,Night,Site,Habitat,Station,Tag,Species_code,Sex,Rep,Status,Weight)

# Join everything together
first.capture.import <- bind_rows(list(first.capture.import0, append2019,
                                       first.capture.2022,first.capture.2023,first.capture.2024)) %>%
  arrange(Year, Site, Habitat, Night, Species_code, Tag)
  
  with(first.capture.import, table(Site, Year, Habitat))

# Make some corrections from Bob Schooley
# See source_data/received_from_Bob_Schooley/Ecotone_Rodent_Capture_BobCorrections.csv

# post-scrotal female should be pregnant female
first.capture.import$Status[!is.na(first.capture.import$Status) & first.capture.import$Sex == "F" & first.capture.import$Status == "PS"] <- "P"
# lactating male should be Rep = "N" and Status == "A"
first.capture.import[first.capture.import$Sex == "M" & first.capture.import$Status == "L" & 
                       !is.na(first.capture.import$Rep) & !is.na(first.capture.import$Status),c("Rep", "Status")] <- list("N", "A")
# scrotal female should be Rep = "N" and Status == "A"
first.capture.import[first.capture.import$Sex == "F" & first.capture.import$Status == "S" & 
                       !is.na(first.capture.import$Rep) & !is.na(first.capture.import$Status),c("Rep", "Status")] <- list("N", "A")

# Pregnant status recorded in Rep instead of Status
first.capture.import[first.capture.import$Rep == "P" & 
                       !is.na(first.capture.import$Rep) & !is.na(first.capture.import$Status),c("Rep", "Status")] <- list("R", "P")

# Change reproductive juveniles to non-reproductive juveniles
first.capture.import$Rep[first.capture.import$Rep == "R" & first.capture.import$Status == "J" &
                           !is.na(first.capture.import$Rep) & !is.na(first.capture.import$Status)] <- "N"

# Change non-reproductive and lactating to reproductive 
first.capture.import$Rep[first.capture.import$Rep == "N" & first.capture.import$Status == "L" &
                           !is.na(first.capture.import$Rep) & !is.na(first.capture.import$Status)] <- "R"

# Change non-reproductive and post-scrotal to reproductive 
first.capture.import$Rep[first.capture.import$Rep == "N" & first.capture.import$Status == "PS" &
                           !is.na(first.capture.import$Rep) & !is.na(first.capture.import$Status)] <- "R"

# Change non-reproductive and scrotal to reproductive 
first.capture.import$Rep[first.capture.import$Rep == "N" & first.capture.import$Status == "S" &
                           !is.na(first.capture.import$Rep) & !is.na(first.capture.import$Status)] <- "R"


# Species codes PEFL and PGFL are for the same species: Perognathus flavus.
# first.capture.import$Species_code[first.capture.import$Species_code == "PEFL"] # <- "PGFL"

# Change missing values of Sex to unknown
first.capture.import$Sex[is.na(first.capture.import$Sex)] <- "U"

range(first.capture.import$Year)
range(first.capture.import$Date)
unique(first.capture.import$Night)
unique(first.capture.import$Site)
unique(first.capture.import$Habitat)
unique(first.capture.import$Station)
unique(first.capture.import$Tag)
unique(first.capture.import$Species_code)
unique(first.capture.import$Species_binomial)
unique(first.capture.import$Common_name)
unique(first.capture.import$Sex)
unique(first.capture.import$Rep)
unique(first.capture.import$Status)
range(first.capture.import$Weight, na.rm = TRUE)
hist(first.capture.import$Weight)
unique(first.capture.import$Comment)

first.capture.import %>%
  distinct(Species_code, Species_binomial, Common_name) %>%
  arrange(Species_code)

obs.check <- first.capture.import %>%
  dplyr::filter(Sex == "M" & Status == "L" | Sex == "F" & Status == "PS" | Sex == "F" & Status == "S"| Rep == "P")

# write_csv(obs.check, "Ecotone_Rodent_Capture_obsToCheck.csv")

# Send to Bob Schooley for review
with(first.capture.import, table(Sex, Status))
with(first.capture.import, table(Rep, Status))

status.check <- first.capture.import %>%
  distinct(Sex, Status) 

obs.check2 <- first.capture.import %>%
  dplyr::filter(Rep == "N" & Status %in% c("L", "PS", "S") |
                  Rep == "R" & Status == "J")

# Notes from Bob Schooley: There have been some taxonomic changes
# NEAL is now Neotoma leucodon (formerly Neotoma albigula)
# CHPE is now Chaetodipus eremicus (formerly Chaetodipus penicillatus) ## But this has not appeared in this data set as of 2021
# SPSP is now Xerospermophilus spilosoma (formerly Spermophilus spilosoma)


# Check for missing data
sapply(first.capture.import, function(x) sum(is.na(x)))

first.capture.import %>% dplyr::filter(is.na(Station))

# Check to make sure Year matches date
first.capture.import %>%
  mutate(year = as.numeric(format(Date, format = "%Y"))) %>%
  dplyr::filter(Year != year)


# Note: Tages are not unique between species
# dup.tags <- first.capture.import %>%
#   dplyr::select(Tag, Species_code) %>%
#   distinct() %>%
#   group_by(Tag) %>%
#   dplyr::summarise(total = n()) %>%
#   dplyr::filter(total > 1)
# range(dup.tags$total)
# 
# dup.tags %>%
#   dplyr::filter(total > 2)
# 
# first.capture.import %>%
#   dplyr::filter(Tag %in% dup.tags$Tag) %>%
#   arrange(Tag, Species_code)
# 
# first.capture.import %>%
#   dplyr::filter(Tag =="2248") %>%
#   arrange(Tag, Species_code)


# Export ecotone.ANPP.fg.final as a csv to 'pathname'
options(scipen=999)   # turns of scientific notation
write.csv(first.capture.import, paste0(out.path, "/", "Ecotone_Rodent_Capture_juntar.csv"),
          quote=F, row.names=F, na = ".")

