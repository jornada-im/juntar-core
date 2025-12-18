library('tidyverse')

# source('../config.R')
# lter_usda_crossref <- read_csv(paste0(core.root,
#   "jornada_im/TaxonomicCoverage/LTER_to_USDA_PLANTS_codes.csv"))

# TODO: Make this search for potential dead (5th digit D) and seedling
# (5th digit S) codes.

# Note that this is modeled after the R code in 210121001
merge_crossref_taxa <- function(df, 
        df_ltercol,
        crossref_path="",
        df_keepcol=NULL,
				crossref_name="LTER_to_USDA_PLANTS_codes.csv",
				crossref_ltercol="LTER_code",
				crossref_codecol="USDA_code"){
  # Load the crossref fild
  crossref <- read_csv(paste(crossref_path, crossref_name, sep='/'))

  # Get a list of unique species codes in the data set
  spp_codes <- df %>%
    dplyr::select(all_of(df_ltercol), all_of(df_keepcol)) %>% 
    dplyr::distinct()

  # Merge unique species list with species codes
  spp_list <- merge(x = spp_codes, y = crossref, 
		    by.x = df_ltercol, 
		    by.y = crossref_ltercol,
		    all.x = TRUE)

  # Verify list of unique codes is the same length as list of merged codes
  codelengthcheck <- nrow(spp_list) == nrow(spp_codes)
  if (codelengthcheck) {
    message(paste(nrow(spp_list), 'LTER codes matched in crossref.'))
  } else {
    message("Need to check something here... lists don't match")
  }

  # Check to see if all species codes are accounted for
  # Get rows where the crossref_codecol, usually USDA_code, is NA (no match)
  codes_unmapped <- spp_list %>% filter(is.na(crossref_codecol))
  
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
    left_join(crossref, by = setNames(crossref_ltercol, df_ltercol))

  # Verify original data has the same number of observations as the merged data
  if (nrow(df) == nrow(df_edit)) {
      message("Returned dataset has the same number of rows")
  } else {
      message("RETURNED DATAFRAME IS NOT THE SAME SIZE!")
  }

  return(list("merged" = df_edit, "unmapped_codes" = codes_unmapped))
}
