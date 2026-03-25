# jrn002001_nppgrg_ppt.R
# formerly: build_dataset.210002001.R
# 

# Set the working directory to a local or network share path
# (this only works in RStudio). 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# If this fails try something like these:
# setwd('/Volumes/unix/path/to/datasets/210.../')
# setwd('Z:\\windows\path\to\datasets\210...\)

library(tidyverse)

# Path to incoming source data files
dsource <- "./source_data/"

# Output data file name
f_out <- "JRN002001_NPP_graduated_rain_gauge_data.csv"

# Load dataset from John
npp_grg <- read_csv(paste0(dsource, "Study002001_GRG-data.csv"),
                    skip=3, na=c('NA', '')) %>%
  rename('gauge_id'=id_gage, 'gauge_loc'=loc_gage, 'gauge_type'=type) %>%
  mutate(owner = recode(owner, lter="JRN_LTER", usda="USDA_JER",
                        cdrrc="CDRRC"))
npp_grg_old <- read_csv(paste0(dsource, "npp_grg.csv"),
                               skip=93, na=c('.','NULL'))


# Not sure why I need to do this one - emlassemblyline thinks the
# \# are delimiters?
npp_grg$gauge_id <- gsub("#","num",npp_grg$gauge_id)
npp_grg$comment <- gsub(",",";",npp_grg$comment)
npp_grg$comment <- gsub('"','in',npp_grg$comment)

# Remove data from SMLM and BIOD sites - these GRGs were added later, they do
# not need to be here not, though we may expand the dataset to include them in
# the future (per John)
#npp_grg <- npp_grg[!(npp_grg$site=="BIOD" | npp_grg$site=="SMLM"),]

df.export <- npp_grg
# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
unique(df.export$zone)
unique(df.export$site)
unique(df.export$qflag)
unique(df.export$gauge_loc)
unique(df.export$gauge_id)
unique(df.export$gauge_type)
unique(df.export$gauge_unit)
unique(df.export$owner)

# Make sure zones are ok
zonesite <- count(df.export, zone, site)

# Count the unique Site gauge ID locations
site_gid <- count(df.export, site, gauge_id)
# I think it should be the same size as
site_gloc <- count(df.export, site, gauge_id, gauge_loc)
# But... check to see if gauges appear in 2 locations
gid_gloc <- count(df.export, gauge_id, gauge_loc)

# Make a simple figure
df_cum <- npp_grg %>% select(c('date', 'site', 'ppt_mm')) %>% 
  mutate(ppt_mm = replace_na(ppt_mm, 0)) %>% group_by(site) %>% 
  mutate(cum_ppt_mm = cumsum(na.omit(ppt_mm)))

fig <- ggplot(data=df_cum, aes(x=date, y=cum_ppt_mm, col=site)) +
  geom_line() + ylab('Cumulative precip (mm)') #+ facet_wrap(~ site, ncol=5)
fig
ggsave('grg_mmprecip_cumulative.png')

# Get a summary of start dates for each site
df_std <- npp_grg %>% select(c('date', 'site')) %>% 
  group_by(site) %>% 
  summarize(stdate = min(date),
            enddate = max(date))

df_std



# Export df.export as a csv to current directory (no rownames or quoting)
options(scipen=999)   # turns of scientific notation
write.csv(df.export, f_out, quote=F, row.names=F)

# Copy the grg
file.copy('metadata_docs/Jornada_002001_npp_precipitation_grg_dsd.txt',
          'npp_GRG_precip_dsd.txt')
