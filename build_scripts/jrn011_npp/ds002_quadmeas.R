# ds002_quadmeas.R
# formerly: build_dataset.210011002.R
# 

library(tidyverse)

source('config.R')
source.path <- paste(im.path, "dataprep/jrn011_npp/field", sep='/')
output.path <- paste(im.path, 'Core_packages/210011002_npp_quadmeas', sep='/')

# Output data file name
f_out <- paste(output.path, "jrn011002_npp_quadrat_meas.test.csv", sep='/')


# Read in data file from Heather that goes to 2017
# Need to figure out origin of this...
df1 <- read_csv(paste(source.path, "Jornada_011002_npp_quad_data.csv", sep='/'),
                skip=40, na = c(".", ""),
                col_types = cols(
                  fileid = col_character(),
                  year = col_double(),
                  season = col_character(),
                  date = col_character(),
                  zone = col_character(),
                  site = col_character(),
                  quad = col_double(),
                  spp = col_character(),
                  obs = col_double(),
                  cover = col_double(),
                  ht = col_double(),
                  cnt = col_double(),
                  phen = col_character(),
                  habit = col_character(),
                  form = col_character(),
                  cpath = col_character(),
                  x = col_double(),
                  y = col_double(),
                  d = col_double(),
                  e = col_double(),
                  comment = col_character())) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

# There are some missing spp codes:
#df1 %>% filter(is.na(spp)) %>% View() # not sure what to do with these

# Check missing data/bareground
# NONE is bareground (e.g. no vegetation)
df1 %>% filter(spp=='NONE') %>% count() # How many NONE?
# How many records with no count and no cover observation?
df1 %>% filter(is.na(cnt) & is.na(cover)) %>% count()
# How many where spp is not NONE
df1 %>% filter(is.na(cnt) & is.na(cover) & spp != 'NONE') # All MISS
df1 %>% filter(spp=='MISS') %>% count() # so is miss the same as NONE?

# Now read in the re-formatted data from the NPP repository - 2018 to present
# These files vary a little in format (columns come and go), and I don't
# really trust the taxonomic columns
count <- 1
for (y in c(2018, 2019, 2020, 2021, 2022, 2023, 2024)){
  fname_w <- paste0(source.path, '/quad/nppq', y, 'w_gm.DAT')
  df_w <- read_delim(fname_w, delim=' ', na = c(".", "", "NA"))
  fname_s <- paste0(source.path, '/quad/nppq', y, 's_gm.DAT')
  df_s <- read_delim(fname_s, delim=' ', na = c(".", "", "NA"))
  fname_f <- paste0(source.path, '/quad/nppq', y, 'f_gm.DAT')
  df_f <- read_delim(fname_f, delim=' ', na = c(".", "", "NA"))
  if (count == 1){
    df2 <- bind_rows(df_w, df_s, df_f)
  } else {
    df2 <- bind_rows(df2, df_w, df_s, df_f)
  }
  count <- count + 1
}
# Reformat df2 a little bit
df2 <- df2 %>% mutate(
  date = as.Date(date,"%m/%d/%Y"),
  year = lubridate::year(date),
  season = toupper(substr(fileid, 7,7)),
  # The following were in estimate_biomass.R from the NPP pipeline
  #cnt = ifelse(is.na(cnt), 1, cnt ),# Bareground counts are 1
  #cover = ifelse(is.na(cover), 0.05, cover)# Missing cover defaults to 0.05 - may need to be turned off for 1989s, 1989f, 1990w, 1990s, and 1990f;
  ) %>%
  select(fileid, year, season, date, everything())

# Check missing data/bareground
# NONE is bareground (e.g. no vegetation)
df2 %>% filter(spp=='NONE') %>% count() # How many NONE?
# How many records with no count and no cover observation?
df2 %>% filter(is.na(cnt) & is.na(cover)) %>% count()
# How many where spp is not NONE?
df2 %>% filter(is.na(cnt) & is.na(cover) & spp != 'NONE') #zero, thats good
# Which records are NA in cover or count
df2 %>% filter(is.na(cover) & spp != 'NONE')
df2 %>% filter(is.na(cnt) & spp != 'NONE')

df.all <- bind_rows(df1, df2) %>%
  select(-form, -habit, -cpath)

# Remove commas in comments column
df.all$comment <- gsub(",", ";", df.all$comment)

# unique values
unique(df.all$season)
unique(df.all$zone)
unique(df.all$site)
unique(df.all$quad)
unique(df.all$phen)
unique(df.all$spp)
unique(df.all$d)
unique(df.all$e)
#unique(df.all$comment)

# any NAs?
sapply(df.all, function(x) sum(is.na(x)))

# Get the taxonomic merge code
## The chdir argument lets the sourced script use relative paths
source('R/taxa_code_merge.R', chdir=TRUE)
tm <- merge_crossref_taxa(df.all, 'spp', paste(im.path, 'TaxonomicCoverage', sep='/'))

unmapped <- tm$unmapped_codes
# Check record...
df.all %>% filter(spp=='PECY')
# SPOR1S = Sporobolus species seedling?
# YUELLS = Yucca elata leaf seedling?

df.export <- tm$merged

# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
unique(df.export$habit)
unique(df.export$form)
unique(df.export$cpath)

# Export df.export as a csv to current directory
options(scipen=999)   # turns of scientific notation
write.csv(df.export, f_out, quote=F, row.names=F)

