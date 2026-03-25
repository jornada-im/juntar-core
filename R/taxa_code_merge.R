library('tidyverse')

# source('../config.R')
# lter_usda_crossref <- read_csv(paste0(core.root,
#   "jornada_im/TaxonomicCoverage/LTER_to_USDA_PLANTS_codes.csv"))

# This function sets paths to the current LTER field codes and USDA codes
getpaths <- function(im_path){
  return(list(current_field_codes = paste(
              im_path, "dataprep", "jrn520_taxa", "fieldcodes",
              "lter_field_codes_20260323.csv", sep="/"),
              current_USDA_codes = paste(
              im_path, "dataprep", "jrn520_taxa", "plants",
              "usda_plantlst_20260318.txt", sep="/")
  ))
}


# Note that this is modeled after the R code in 210121001

# Formerly called `merge_crossref_taxa`
match_lter_codes <- function(df, 
        df_ltercol,
        im_path = "",
        df_keepcol = NULL, # An additional set of colums to retain in the output
				crossref_matchcol = "field_code",
				crossref_authcol = "usda_code"){
  # Get the paths
  path <- getpaths(im_path)
  # Load the LTER field codes (lter_field_codes_YYYYMMDD) file, which lists all
  # known field codes and links them to accepted LTER codes and taxonomic
  # authority codes.
  crossref <- read_csv(path$current_field_codes, na=c("",".","NA"), skip=2) %>%
    select(!(is_cover:comment))

  # Get a list of unique JRN LTER species codes in the data set
  spp_codes <- df %>%
    dplyr::select(all_of(df_ltercol), all_of(df_keepcol)) %>% 
    dplyr::distinct()

  # Merge unique JRN species codes with the LTER field codes
  spp_list <- left_join(spp_codes, crossref,
    by=join_by({{df_ltercol}}=={{crossref_matchcol}}), keep=F) |>
    arrange(spp)

  # Verify list of unique codes is the same length as list of merged codes
  codelengthcheck <- nrow(spp_list) == nrow(spp_codes)
  if (codelengthcheck) {
    message(paste(nrow(spp_list), 'LTER codes matched in crossref.'))
  } else {
    message("Need to check something here... lists don't match")
  }

  # Check to see if all species codes are accounted for
  # Get rows where the crossref_authcol, usually USDA_code, is NA (no match)
  codes_unmapped <- spp_list %>% filter(is.na(crossref_authcol))
  
  message(paste("There are", nrow(codes_unmapped), "unmapped lter codes"))

  # Check to see if path, habit, are form are the same for each code
  # data %>%
  #  dplyr::select(spp, path, habit, form) %>%
  #  distinct() %>%
  #  group_by(spp) %>%
  #  summarise(total = n()) %>%
  #  dplyr::filter(total > 1)
  # They are not the same!!
  # Solution: Use path, habit, and form from the LTER-to-USDA codes file

  # Edit the data set for posting online
  # Add in the USDA codes and species binomials
  df_edit <- df %>% #select(-path, -habit, -form) %>%
    # There is standard evaluation error with using variables for column names
    # See: https://stackoverflow.com/questions/28125816/r-standard-evalation-for-join-dplyr
    left_join(crossref, by = setNames(crossref_matchcol, df_ltercol))

  # Verify original data has the same number of observations as the merged data
  if (nrow(df) == nrow(df_edit)) {
      message("Returned dataset has the same number of rows")
  } else {
      message("RETURNED DATAFRAME IS NOT THE SAME SIZE!")
  }

  return(list("plant_list" = spp_list, "unmapped_codes" = codes_unmapped, "merged" = df_edit))
}


# Formerly called `merge_usdaplants_taxa`
match_usda_codes <- function(df, 
        df_usdacol,
        im_path="",
        df_keepcol=NULL, # An additional set of colums to retain in the output
				crossref_matchcol="Symbol",
				crossref_authcol="Symbol"){
  path <- getpaths(im_path)
  # Load the USDA Plants file. This file lists all known USDA codes 
  # codes and related taxonomic info
  crossref <- read_csv(path$current_USDA_codes, na=c("",".","NA"))

  # Get a list of unique (presumed) USDA species codes in the data set
  spp_codes <- df %>%
    dplyr::select(all_of(df_usdacol), all_of(df_keepcol)) %>% 
    dplyr::distinct()


  # Merge unique USDA codes with what is in USDA Plants
  spp_list <- left_join(spp_codes, crossref,
    by=join_by({{df_usdacol}}=={{crossref_matchcol}}), keep=TRUE)

  # Verify list of unique codes is the same length as list of merged codes
  codelengthcheck <- nrow(spp_list) == nrow(spp_codes)
  if (codelengthcheck) {
    message(paste(nrow(spp_list), 'USDA codes matched in crossref.'))
  } else {
    message("Need to check something here... lists don't match")
  }

  # Check to see if all species codes are accounted for
  # Get rows where the crossref_authcol, usually USDA_code, is NA (no match)
  codes_unmapped <- spp_list %>% filter(is.na(crossref_authcol))
  
  message(paste("There are", nrow(codes_unmapped), "unmapped lter codes"))

  # Edit the data set for posting online
  # Add in the USDA codes and species binomials
  df_edit <- df %>% #select(-path, -habit, -form) %>%
    # There is standard evaluation error with using variables for column names
    # See: https://stackoverflow.com/questions/28125816/r-standard-evalation-for-join-dplyr
    left_join(crossref, by = setNames(crossref_matchcol, df_usdacol))

  # Verify original data has the same number of observations as the merged data
  if (nrow(df) == nrow(df_edit)) {
      message("Returned dataset has the same number of rows")
  } else {
      message("RETURNED DATAFRAME IS NOT THE SAME SIZE!")
  }

  return(list("plant_list" = spp_list, "unmapped_codes" = codes_unmapped, "merged" = df_edit))
}
