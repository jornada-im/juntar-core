library(tidyverse)
library(readxl)
source("./config.R")

"""
Joining algorithm

There are multiple potential merge keys: lter_code, usda_code, sciname, sciname_auth

Output 1: Plant list with columns lter_code, usda_code, sciname, sciname_auth
Output 2: Synonyms with columns lter_code, usda_code, usda_code_synonym, sciname_auth
Output 3: Traits list?

Steps (1-4 done prior to this script)

1. Standardize and rename column headers in`main` and `john`, and standardize on "subsp." for infraspecific rank.
2. Drop generic and higher taxa (length of `lter_code` > 4) from `john`
3. Drop unnecessary columns in `main` list (Genus_USDA, Species_USDA, further_rank_USDA)
4. Create a `sciname_auth` column in the `allred` table by joining columns
5. For `main` and `john` lists, evaluate unique lter and usda codes, and correspondence between codes in each file
6. Examine where `sciname_auth` (derived from Citation_LTER originally) is missing and whether genus_lter/species_lter can fill it (none)
7. Full join `main` to species rows from `john` on `lter_code` to make `main_new`
    - Create a `sciname_auth` from `sciname_auth_john` first, then `sciname_auth_main. Leave the rest NA for now
    - Denote source of `sciname_auth` in new column `sciname_auth_src`
    - Create match columns for `family_main/john`, `usda_code_main/john`, `habit_main/john`, `form_main/john`, and `cpath_main/john`.
8. Examine where `usda_code`, `family`, `habit`, `form`, `cpath` don't match between `main` and `john`
    - Family and habit already match where present in each, made minor edits to form and cpath in `john`
    - There were about 60 non-matching usda codes between `main` and `john` - these were manually edited in source files and then source is selected.
    - Create family, habit, form, and cpath from `john`, then `main`, then leave others NA.
9. Verify `usda_code` values in `main_new` are valid and identify which are synonyms.
10. Merge in sciname_auth and LTER/USDA codes from USDA plants and Allred taxonomy files
    - for all empty sciname_auth, sciname & common_name values in main_new retrieve values from corresponding usda columns
11. Verify when sciname taxonomy matches usda vs allred
11. Fix conflicts between `usda_codes` in `john` and `main` `sciname` and `sciname_auth` conflicts with USDA plants for any given `usda_code` (as describe by `sciname_usda_match` and `sciname_auth_usda_match`). 
    - Reason 1: `sciname_auth` refers to outdated taxonomy that should be moved to `alias` and replaced with the value in `sciname_auth_usda` (also replace `sciname` with `sciname_usda`).
    - Reason 2: `sciname_auth` refers to an infraspecific taxa that `usda_code` does not. If a code for that taxon exists look in `usda_code_main` or `usda_code_john` for one to replace it. If no appropriate code is found in `usda`, make a note that the taxa is not accepted by USDA Plants.
    - Reason 3: `sciname_auth` has a different infraspecific rank abbreviation (.ssp vs .subsp). Replace the abbreviation in `sciname_auth` with the one in `sciname_auth_usda`.


from USDA codes that fuzzy match John's synonyms, then Allred synonyms if different
"""

# Path to the plant list files (jrn520)
plants_path <- paste(im_path, "dataprep", "jrn520_taxa", "plants", sep="/")

## Primary Jornada plant list - the previous version
main <- read_csv(file.path(plants_path, "archive/jrn_plant_list_MAIN_20260526.csv"),
                     skip=4, na = c(".", "NA","")) |>
  rename_with(tolower) |>
  rename(cpath=pathway, sciname_auth=citation_lter, lter_observed=lter_core) |> 
  mutate(sciname_auth = str_replace(sciname_auth, " ssp. ", " subsp. ")) |>
  select(-genus_usda, -species_usda, -further_rank_usda)

# Temporary for agent sandbox
#write_csv(main, "~/Desktop/agent-sandbox-io/plant_data/main_plant_list.csv")

## John Anderson's list
john <- read_excel(file.path(plants_path, "0_test-merge1_20260123.xlsx"),
                        skip=2, na = c(".", "NA")) |>
  rename_with(tolower) |>
  rename(lter_code=lter_spp, usda_code=`current usda code`, usda_code_old=`direct usda code`,
         sciname_auth=lter_full_species_name) |>
  # Get rid of non-plant codes and one weird genus code
  filter(nchar(lter_code) < 5 & lter_code!="NONE" & lter_code!="MISS" & lter_code!="MAMI") |>
  mutate(sciname_auth = str_replace(sciname_auth, " ssp. ", " subsp. "))

# Temporary for agent sandbox
#write_csv(john, "~/Desktop/agent-sandbox-io/plant_data/johns_plant_list.csv")

## Kelly Allred's Flora of the Jornada Plain list - this is a lightly edited version
# of the Claude output
allred <- read_csv(file.path(plants_path, "allred_jornada_spp_table_claude_v4.csv"),
                           na = c(".", "NA",""))# |>
# Temporary for agent sandbox
#write_csv(allred, "~/Desktop/agent-sandbox-io/plant_data/allred_plant_list.csv")

## USDA Plants
usda <- read_csv(file.path(plants_path, 'usda_plantlst_20260318.txt')) |>
  rename_with(tolower) |>
  rename(usda_code = symbol, usda_code_syn = `synonym symbol`,
         sciname_auth = `scientific name with author`) |>
  mutate(sciname_auth = str_replace(sciname_auth, " ssp. ", " subsp. "))

## Identify duplicate lter_codes in main
main_dup_ltercode <- main |> count(lter_code) |> filter(n > 1)
cat("Duplicate lter_codes in main:", nrow(main_dup_ltercode), "\n")
if (nrow(main_dup_ltercode) > 0) print(main_dup_ltercode)
# Duplicate lter_codes in main: 12
# A tibble: 12 × 2
#   lter_code     n
#   <chr>     <int>
# 1 ACCO          2
# 2 BOCC          2
# 3 BOCU          2
# 4 CHLI          2
# 5 ECTR          2
# 6 ERCU          2
# 7 MESC          2
# 8 PRGL          2
# 9 SILE          2
#10 TECO          2
#11 XAST          2
#12 NA            5

sum(is.na(main$lter_code))
# Missing lter_codes in main: 5 (all spore plants with lter code conflicts)

## Duplicates in john
john_dup_ltercode <- john |> count(lter_code) |> filter(n > 1)
cat("Duplicate lter_codes in john:", nrow(john_dup_ltercode), "\n")
if (nrow(john_dup_ltercode) > 0) print(john_dup_ltercode)
# Duplicate lter_codes in john: 0 

sum(is.na(john$lter_code))
# Missing lter_codes in john: 0

## Correspondence between main and john lter_codes
main_lter_codes <- main$lter_code
john_lter_codes <- john$lter_code

cat("\nIn main:", length(main_lter_codes) - length(setdiff(main_lter_codes, john_lter_codes)),
    "lter codes overlap with john (including duplicates), and",
    length(setdiff(main_lter_codes, john_lter_codes)), "do not.\n")
cat("\nIn john:", length(john_lter_codes) - length(setdiff(john_lter_codes, main_lter_codes)),
    "lter codes overlap (intersect) with main, and",
    length(setdiff(john_lter_codes, main_lter_codes)), "do not.\n")
cat("\nThere are", sum(is.na(main$lter_code)),
    "NA values for lter_code in main (",sum(is.na(john$lter_code)),"in john) \n")
# In main: 281 lter codes overlap with john (including duplicates), and 287 do not. 
# In john: 266 lter codes overlap with main, and 5 do not.
# There are 5 NA values for lter_code in main ( 0 in john)

# Count taxa
in_main_only <- setdiff(na.omit(main_lter_codes), john_lter_codes) #omit NA lter_codes in main for now
# Long list (286), as expected as this is the more inclusive list
in_john_only <- setdiff(john_lter_codes, main_lter_codes)
# "ARGL" "ERAR" "ERPU" "HIMU" "PAFA"
in_both <- intersect(john_lter_codes, main_lter_codes)
# 266 codes in both
length(union(main_lter_codes, john_lter_codes))
length(c(in_both, in_main_only, in_john_only)) + sum(is.na(main$lter_code))
# 558 LTER codes (including NA) and total of 562 taxa in both lists, 
# NOT counting duplicate lter codes in main

# Look at sciname_auth and usda_code now
# The main list is somewhat incomplete and has duplicate usda codes. ----
main_missing_sciname <- main |> filter(is.na(sciname_auth)) |> 
  select(lter_code, sciname_auth_main=sciname_auth)
cat("Rows in main with no sciname_auth:", nrow(main_missing_sciname), "\n")
main_dup_usdacode <- main |> filter(duplicated(usda_code))
cat("Duplicate USDA codes in main:", nrow(main_dup_usdacode), "\n")
main_missing_usdacode <- main |> filter(is.na(usda_code))
cat("Missing USDA codes in main:", nrow(main_missing_usdacode), "\n")
# Rows in main with no sciname_auth: 192 
# Duplicate USDA codes in main: 8
# Missing USDA codes in main: 0 

# John's list is somewhat incomplete and has duplicate usda codes. ----
john_missing_sciname <- john |> filter(is.na(sciname_auth)) |>
  select(lter_code, sciname_auth_john=sciname_auth)
cat("Rows in john with no sciname_auth:", nrow(john_missing_sciname), "\n")
john_dup_usdacode <- john |> filter(duplicated(usda_code))
cat("Duplicate USDA codes in john:", nrow(john_dup_usdacode), "\n")
john_missing_usdacode <- john |> filter(is.na(usda_code))
cat("Missing USDA codes in john:", nrow(john_missing_usdacode), "\n")
# Rows in john with no sciname_auth: 5  
# Duplicate USDA codes in john: 7 
# Missing USDA codes in john: 0 

# Define some columns to drop during a join
drop_main <- c("genus_lter","species_lter")
drop_john <- c(colnames(john)[grep("gm_", colnames(john))],"lter_genus","lter_species","lter_status","lter_citation_ext","lter_stat")

# JOIN MAIN AND JOHN tables
# note there are no NAs in john, so total should be nrows(main) + nrows(in_john_only)
main_new <- main |> select(-all_of(drop_main)) |>
  full_join(john |> select(-all_of(drop_john)),
  by = "lter_code", suffix = c("_main","_john"))

# Check for missing values in sciname, lter, and usda codes
main_new |> summarise(across(starts_with(c("sciname_", "usda_", "lter_c")), ~ sum(is.na(.))))
# A tibble: 1 × 6
#   sciname_auth_main sciname_auth_john usda_code_main usda_code_old usda_code_john lter_code
#               <int>             <int>          <int>         <int>          <int>     <int>
# 1               200               300              5           295            295         5
# sciname_auth_main: 198 from main plus 5 rows from john
# sciname_auth_john: 286 (in_main_only), +5 unfilled dups from main, 5 NAs from main, + 4 NAs in john

## Are the same duplicate lter_codes in main_new?
main_new_dup_ltercode <- main_new |> count(lter_code) |> filter(n > 1)
cat("Duplicate lter_codes in main:", nrow(main_dup_ltercode), "\n")
if (nrow(main_new_dup_ltercode) > 0) print(main_new_dup_ltercode)
identical(main_dup_ltercode, main_new_dup_ltercode)
# Yes

# Now lets fill in combined columns
main_new <- main_new |>
  mutate(
    # Lowercase common names (all from John)
    common_name = tolower(common_name),
    # Denote source of sciname_auth
    sciname_auth_src = case_when(
    is.na(sciname_auth_main) & !is.na(sciname_auth_john) ~ "J.Anderson", #ARFE and ARGL
    !is.na(sciname_auth_main) & !is.na(sciname_auth_john) ~ "Main",
    !is.na(sciname_auth_main) & is.na(sciname_auth_john) ~ "Main",
    is.na(sciname_auth_main) & is.na(sciname_auth_john) ~ "USDA Plants"),
    # Choose sciname_auth from john, then main, the rest NA until merging in other sources
    sciname_auth = case_when(
      is.na(sciname_auth_main) & !is.na(sciname_auth_john) ~ sciname_auth_john,
      !is.na(sciname_auth_main) & !is.na(sciname_auth_john) ~ sciname_auth_main,
      !is.na(sciname_auth_main) & is.na(sciname_auth_john) ~ sciname_auth_main,
      is.na(sciname_auth_main) & is.na(sciname_auth_john) ~ NA))

# Check how well USDA codes, scinames, families, and traits match
sum(main_new$usda_code_main!=main_new$usda_code_john, na.rm=TRUE)   #60 43
sum(main_new$sciname_auth_main!=main_new$sciname_auth_john, na.rm=TRUE) #12 38
sum(main_new$family_main!=main_new$family_john, na.rm=TRUE) # 0
sum(main_new$habit_main!=main_new$habit_john, na.rm=TRUE) # 0
sum(main_new$form_main!=main_new$form_john, na.rm=TRUE) # 0 - was 2, updated ZELO and CAHU in source lists
sum(main_new$cpath_main!=main_new$cpath_john, na.rm=TRUE) # 0 - was 15, updated CM to CAM in `john`

# Look at where usda_code values don't match in `main` and `john` (the top number above)
non_matching_usda <- main_new[
  if_else(main_new$usda_code_main!=main_new$usda_code_john,TRUE, FALSE,missing=FALSE),
  c("lter_code","sciname_auth","usda_code_main","usda_code_john","sciname_auth_main","sciname_auth_john")]
# In many cases these are because John has chosen infraspecific codes that `main` did not use
# (for example, AMCR, APRA, ARFE, ARHA, ARLO, ARLU, ARNE, ARPE, ARPU, ARTE, ARWR, ASMO)
# Codes in main may have been updated independently of john's list - resolve below.

# Now look at where sciname_auth values don't match in `main` and `john` (2nd number above)
non_matching_sciname_auth <- main_new[
  if_else(main_new$sciname_auth_main!=main_new$sciname_auth_john,TRUE, FALSE,missing=FALSE),
  c("lter_code","sciname_auth","sciname_auth_main","sciname_auth_john","usda_code_main","usda_code_john")]
# Total is 38 non-matching, but there are also taxa that don't co-occur (ARGL), and taxa
# where one sciname_auth is missing (ARFE for example). Resolve below

# After manual review and editing, use these sources for usda_code for the following LTER codees
use_john <- c("AMCR", "APRA","ARFE","ARHA","ARLO","ARLU","ARNE","ARPE","ARPU","ARTE","ARWR",
              "ASWO", "BAAB","ASMO", "ASNU","BOTO","BRFA","CONI","DEPI","DRCU","DYPE","ECFE",
              "ERME","ERPE","ESME","EUSR","GAPA","GIFL","HOMU","HYFL","LARE","LEMO","LEVI",
              "LUCO","MAGU","MUSQ","PHAC","SELO","STAR","VEWR","ARGL")
use_main <- c(non_matching_sciname_auth$lter_code, "COMA","COVI","EUDE","MALI","MEPU","MILI",
              "SAKA","TAAN","TAAU","ARDI", "ARPA", "ARGA","ERMI","SACY","SAHA","PORE","POOL",
              "OPVI","OPDI","OPPH","OPIM","OPLE","OPSP","LEMO","DISP","CRPO","CRJA","HAGR","EUMI",
              "POWI")

non_matching_usda$lter_code %in% c(use_main, use_john)
#  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# [22] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# [43] TRUE TRUE TRUE

# Select usda_code based on the use_X list, then john, then main
main_new <- main_new |> mutate(
  usda_code = case_when(
      lter_code %in% use_john ~ usda_code_john,
      lter_code %in% use_main ~ usda_code_main,
      !is.na(usda_code_john) ~ usda_code_john,
      !is.na(usda_code_main) & is.na(usda_code_john) ~ usda_code_main,
      is.na(usda_code_main) & is.na(usda_code_john) ~ NA))
  
# Now consolidate main/john versions of `family`, `habit`, `form` and `cpath`
main_new <- main_new |> 
  mutate(
    # Choose family from john, then main, the rest NA
    family = case_when(
      !is.na(family_john) ~ family_john,
      !is.na(family_main) & is.na(family_john) ~ family_main,
      is.na(family_main) & is.na(family_john) ~ NA),
    # Choose habit from john, then main, the rest NA
    habit = case_when(
      !is.na(habit_john) ~ habit_john,
      !is.na(habit_main) & is.na(habit_john) ~ habit_main,
      is.na(habit_main) & is.na(habit_john) ~ NA),
    # Choose form from john, then main, the rest NA
    form = case_when(
      !is.na(form_john) ~ form_john,
      !is.na(form_main) & is.na(form_john) ~ form_main,
      is.na(form_main) & is.na(form_john) ~ NA),
    # Choose cpath from john, then main, the rest NA
    cpath = case_when(
      !is.na(cpath_john) ~ cpath_john,
      !is.na(cpath_main) & is.na(cpath_john) ~ cpath_main,
      is.na(cpath_main) & is.na(cpath_john) ~ NA)
    )

# Now lets look at USDA codes and synonyms, and Allred scinames ----
# First get usda codes (accepted and synonyms), sciname_auth, and common_name 
# matching usda codes from main
usda_mapping <- 
  bind_rows(usda |> filter(usda_code_syn %in% main_new$usda_code),
            usda |> filter(is.na(usda_code_syn) & usda_code %in% main_new$usda_code)) |>
  select(usda_code, usda_code_syn, sciname_auth_usda = sciname_auth, common_name_usda = `common name`)

# Now allred
allred_mapping <- 
  bind_rows(allred |> filter(lter_code %in% main_new$lter_code)) |>
  mutate(common_names = tolower(common_names)) |>
  select(lter_code_allred = lter_code, sciname_allred = sciname, sciname_auth_allred = sciname_auth, common_name_allred = common_names)
# Now remove duplicate codes for merging later
allred_dups <- allred_mapping |> filter(duplicated(lter_code_allred))
allred_mapping <- allred_mapping |> filter(!lter_code_allred %in% allred_dups$lter_code_allred)

# Check for duplicated usda and lter codes
usda_mapping[duplicated(usda_mapping$usda_code), ] # These have synonyms already
allred_mapping[duplicated(allred_mapping$lter_code_allred), ]
# These are mostly where LTER codes arent resolved to infraspecific taxa

# Check: are all main_new usda_codes present in usda? ----
all_usda <- unique(c(usda_mapping$usda_code, usda_mapping$usda_code_syn))
sum(main_new$usda_code %in% all_usda) 
# All 573 usda_codes from main_new are found in usda
# Are all lter_codes found in allred?
sum(main_new$lter_code %in% allred_mapping$lter_code_allred)
# No!

# Now identify usda_codes in main that are synonyms
main_new <- main_new |>
  mutate(usda_code_is_syn = usda_code %in% usda_mapping$usda_code_syn)
cat("usda_codes that are synonyms:",
    sum(main_new$usda_code_is_syn, na.rm = TRUE), "\n")
# usda_codes that are synonyms: 28
# Show synonyms
main_new |> filter(usda_code_is_syn) |>
  select(lter_code, usda_code, sciname_auth, usda_code_is_syn)

# For synonym and non-synonym usda_codes in main, retrieve sciname_auth_usda and 
# common_name_usda, and  from usda_mapping ----
main_new <- main_new |>
  # First accepted usda rows (drop synonym rows to prevent dups)
  left_join(usda_mapping |> filter(is.na(usda_code_syn)),
            by="usda_code") |>
  # Now join usda on synonym rows
  left_join(usda_mapping, by=c("usda_code" = "usda_code_syn")) |>
  mutate(
    # Create a column that drops authorities and has just species binomial and infraspecific
    sciname = {
      base  <- str_extract(sciname_auth, "^[A-Z][a-z-]+ [a-z×-]+")
      infra <- str_extract(sciname_auth, "(?<= )(?:subsp\\.|ssp\\.|var\\.|f\\.|forma) [a-z-]+")
      if_else(is.na(infra), base, paste(base, infra))
    },
    # Coalesce sciname_auth_usda and common_name_usda
    sciname_auth_usda = coalesce(sciname_auth_usda.y, sciname_auth_usda.x),
    # Drop authority for sciname_usda now
    sciname_usda = {
      base  <- str_extract(sciname_auth_usda, "^[A-Z][a-z-]+ [a-z×-]+")
      infra <- str_extract(sciname_auth_usda, "(?<= )(?:subsp\\.|ssp\\.|var\\.|f\\.|forma) [a-z-]+")
      if_else(is.na(infra), base, paste(base, infra))
    },
    common_name_usda = coalesce(common_name_usda.y, common_name_usda.x)) |>
  select(-ends_with(c('.x', '.y')))

# Now join allred the same way
# Note that joining on lter code adds rows if not de-duplicated...
main_new <- main_new |>
  left_join(allred_mapping, by=c("lter_code" = "lter_code_allred"))#, relationship="many-to-many")
  # Can also try sciname
  #left_join(allred_mapping, by=c("sciname" = "sciname_allred"))

# Now for all empty sciname_auth, sciname & common_name values in main_new, 
# retrieve values from corresponding usda columns ----
main_new <- main_new |>
  mutate(sciname_auth = if_else(is.na(sciname_auth), sciname_auth_usda, sciname_auth),
        sciname = if_else(is.na(sciname), sciname_usda, sciname),
        common_name = if_else(is.na(common_name), common_name_usda, common_name),
        # Add column indicating which taxonomy sciname_auth agrees with - sciname_usda,
        # sciname_allred, or other
        # Note that, often, USDA and Allred effectively agree
        sciname_auth_follows = case_when(
          sciname_auth==sciname_auth_usda ~ "USDA Plants",
          sciname_auth==sciname_auth_allred ~ "Allred",
          .default = "Other"),
        # This indicates when there are different taxa in USDA
        sciname_usda_match = sciname==sciname_usda,
        sciname_allred_match = sciname==sciname_allred)

# Evaluate issues when sciname does not match sciname_usda
nonmatching <- main_new |> filter(!sciname_usda_match) |>
  select(lter_code, usda_code, sciname, sciname_auth, sciname_auth_main, sciname_auth_john, sciname_auth_usda,
    sciname_auth_allred, sciname_auth_follows, sciname_usda_match, sciname_allred_match, usda_code_is_syn)

# Resolve conflicts between USDA, Allred, and other taxonomies
# For taxa not matching sciname_usda, if taxonomy follows Allred, make the USDA sciname_auth an alias
main_new <- main_new |>
  mutate(
    # First edit BOCC and XAST - dropped due to many-to-many issue
    sciname_auth_follows = replace_when(sciname_auth_follows, lter_code %in% c("BOCC","XAST") ~ "Allred"),
    sciname_auth_allred = replace_when(sciname_auth_allred, lter_code %in% c("BOCC","XAST") ~ sciname_auth),
    alias = replace_when(alias,
      !sciname_usda_match & sciname_auth_follows == "Allred" & !is.na(sciname_auth_usda) & !is.na(alias) ~
        paste(alias, sciname_auth_usda, sep = ";"),
      !sciname_usda_match & sciname_auth_follows == "Allred" & !is.na(sciname_auth_usda) & is.na(alias) ~
        sciname_auth_usda)
    )

# For taxa that follow neither USDA nor Allred ("Other"), move sciname_auth
# to alias and replace it with sciname_auth_usda
main_new <- main_new |>
  mutate(
    alias = replace_when(alias,
      !sciname_usda_match & sciname_auth_follows == "Other" & !is.na(sciname_auth) & !is.na(alias) ~
        paste(alias, sciname_auth, sep = ";"),
      !sciname_usda_match & sciname_auth_follows == "Other" & !is.na(sciname_auth) & is.na(alias) ~
        sciname_auth),
    sciname_auth = replace_when(sciname_auth,
      !sciname_usda_match & sciname_auth_follows == "Other" & !is.na(sciname_auth_usda) ~
        sciname_auth_usda),
    # Also change sciname_auth_follows to reflect the change
    sciname_auth_follows = replace_when(sciname_auth_follows,
      !sciname_usda_match & sciname_auth_follows == "Other" ~ "USDA Plants",
      usda_code_is_syn & sciname_allred_match ~ "Allred",
      usda_code_is_syn & !sciname_allred_match ~ "Other")
  )

# Rebuild sciname from updated sciname_auth and refresh sciname_usda_match
main_new <- main_new |>
  mutate(
    sciname = {
      base  <- str_extract(sciname_auth, "^[A-Z][a-z-]+ [a-z×-]+")
      infra <- str_extract(sciname_auth, "(?<= )(?:subsp\\.|ssp\\.|var\\.|f\\.|forma) [a-z-]+")
      if_else(is.na(infra), base, paste(base, infra))
    },
    sciname_usda_match = sciname == sciname_usda
  )

# Evaluate issues when sciname does not match sciname_usda
nonmatching <- main_new |> filter(!sciname_usda_match) |>
  select(lter_code, usda_code, sciname, sciname_auth, sciname_auth_main, sciname_auth_john, sciname_auth_usda,
    sciname_auth_allred, sciname_auth_follows, sciname_usda_match, sciname_allred_match, usda_code_is_syn)

## Now populate common_name and alias

# When usda_code is a synonym, add the accepted sciname_auth from USDA to alias.
# usda_code_syn -> accepted usda_code -> accepted sciname_auth (where usda_code_syn is NA)
syn_to_accepted_sciname <- usda |>
  filter(usda_code_syn %in% main_new$usda_code[main_new$usda_code_is_syn]) |>
  select(usda_code_syn, accepted_usda_code = usda_code) |>
  left_join(usda |> filter(is.na(usda_code_syn)) |> select(usda_code, sciname_auth_accepted = sciname_auth),
            by = c("accepted_usda_code" = "usda_code")) |>
  select(usda_code_syn, sciname_auth_accepted)

main_new <- main_new |>
  left_join(syn_to_accepted_sciname, by = c("usda_code" = "usda_code_syn")) |>
  mutate(alias = case_when(
    usda_code_is_syn & !is.na(sciname_auth_accepted) & !is.na(alias) ~
      paste(alias, sciname_auth_accepted, sep = ";"),
    usda_code_is_syn & !is.na(sciname_auth_accepted) & is.na(alias) ~
      sciname_auth_accepted,
    .default = alias
  )) |>
  select(-sciname_auth_accepted)

# Add sciname_auth_usda and sciname_auth_allred to alias when they differ from sciname_auth
main_new <- main_new |>
  mutate(alias = case_when(
    !is.na(sciname_auth_usda) & sciname_auth_usda != sciname_auth & !is.na(alias) ~
      paste(alias, sciname_auth_usda, sep = ";"),
    !is.na(sciname_auth_usda) & sciname_auth_usda != sciname_auth & is.na(alias) ~
      sciname_auth_usda,
    .default = alias
  )) |>
  mutate(alias = case_when(
    !is.na(sciname_auth_allred) & sciname_auth_allred != sciname_auth & !is.na(alias) ~
      paste(alias, sciname_auth_allred, sep = ";"),
    !is.na(sciname_auth_allred) & sciname_auth_allred != sciname_auth & is.na(alias) ~
      sciname_auth_allred,
    .default = alias
  ))

# Add common_name_usda and common_name_allred to common_name when not already present
main_new <- main_new |>
  mutate(common_name = case_when(
    !is.na(common_name_usda) & !is.na(common_name) &
      !str_detect(common_name, fixed(tolower(common_name_usda))) ~
      paste(common_name, common_name_usda, sep = ";"),
    !is.na(common_name_usda) & is.na(common_name) ~ common_name_usda,
    .default = common_name
  )) |>
  rowwise() |>
  mutate(common_name = {
    if (is.na(common_name_allred)) {
      common_name
    } else {
      candidates <- tolower(str_trim(str_split_1(common_name_allred, ";")))
      to_add <- if (is.na(common_name)) candidates
                else candidates[!str_detect(common_name, fixed(candidates))]
      if (length(to_add) == 0) common_name
      else if (is.na(common_name)) paste(to_add, collapse = ";")
      else paste(c(common_name, to_add), collapse = ";")
    }
  }) |>
  ungroup() |>
  mutate(across(c(alias, common_name), \(x) {
    if_else(is.na(x), x,
      sapply(str_split(x, ";"), \(parts) {
        parts <- str_trim(parts)
        paste(parts[!duplicated(tolower(parts))], collapse = ";")
      }))
  }))

# Now create a synonyms table.
# Helper to strip authority from sciname_auth — reused in both lookups below
derive_sciname <- function(x) {
  base  <- str_extract(x, "^[A-Z][a-z-]+ [a-z×-]+")
  infra <- str_extract(x, "(?<= )(?:subsp\\.|ssp\\.|var\\.|f\\.|forma) [a-z-]+")
  if_else(is.na(infra), base, paste(base, infra))
}

# Lookup 1: USDA synonym rows — sciname -> usda_code_syn
usda_syn_lookup <- usda |>
  filter(!is.na(usda_code_syn)) |>
  mutate(sciname_syn = derive_sciname(sciname_auth)) |>
  select(usda_code_syn, sciname_syn, sciname_auth)

# Lookup 2: USDA accepted rows — sciname -> usda_code (used when usda_code_is_syn is TRUE)
usda_accepted_lookup <- usda |>
  filter(is.na(usda_code_syn)) |>
  mutate(sciname_syn = derive_sciname(sciname_auth)) |>
  select(usda_code_accepted = usda_code, sciname_syn, sciname_auth)

synonyms <- main_new |>
  filter(!is.na(alias)) |>
  select(usda_code, lter_code, sciname_auth, alias, usda_code_is_syn) |>
  separate_longer_delim(alias, delim = ";") |>
  mutate(
    alias = str_trim(alias),
    sciname_alias = derive_sciname(alias)
  ) |>
  left_join(usda_syn_lookup, by = c("sciname_alias" = "sciname_syn")) |>
  left_join(usda_accepted_lookup, by = c("sciname_alias" = "sciname_syn")) |>
  mutate(usda_code_syn = case_when(
    !is.na(usda_code_syn) ~ usda_code_syn,
    usda_code_is_syn & !is.na(usda_code_accepted) ~ usda_code_accepted,
    .default = NA
  )) |>
  select(usda_code, lter_code, sciname_auth = alias, usda_code_syn)

# Source 2: usda_code_old (from john) differs from accepted usda_code
syn_old_code <- main_new |>
  filter(!is.na(usda_code_old), usda_code_old != usda_code) |>
  # join synonyms from USDA to get current usda code
  left_join(usda_syn_lookup, by = c("usda_code_old" = "usda_code_syn")) |>
  left_join(usda_accepted_lookup, by = c("usda_code_old" = "usda_code_accepted")) |>
  mutate(sciname_auth_test = coalesce(sciname_auth.y, sciname_auth)) |>
  #left_join(usda, by=c("usda_code_old"="usda_code_syn")) |>
  # Get synonym columns
  select(usda_code, lter_code, usda_code_syn = usda_code_old, 
    sciname_auth=sciname_auth_test)

synonyms <- synonyms |>
  bind_rows(syn_old_code) |>
  arrange(lter_code) |>
  distinct()


write_csv(synonyms, file.path(plants_path, "jrn_plant_list_MERGE_SYNONYMS_20260527.csv"))

# Now lets create the new plant list. Differences in USDA codes will
# still need to be resolved, so preserving comparison columns
main_out <- main_new |>
  arrange(sciname) |>
  select(family, sciname, sciname_auth, usda_code, lter_code, common_name, habit, form, cpath,
    nativity, habitat, phenology, reproduction, lter_observed, sciname_auth_follows, usda_code_is_syn, note)#,

write_csv(main_out, file.path(plants_path, "jrn_plant_list_MERGE_MAIN_20260527.csv"))

## TESTS

# Check: Are there duplicate usda codes?
usda_code_dup <- main_out |>
  filter(!is.na(usda_code)) |>
  group_by(usda_code) |> filter(n() > 1) |>
  ungroup() |> arrange(usda_code) |>
  select(usda_code, lter_code, sciname_auth)
cat("Duplicate usda_codes in main_out:", nrow(usda_code_dup), "\n")
if (nrow(usda_code_dup) > 0) print(usda_code_dup)

# Check: Are there duplicate lter codes?
lter_code_dup <- main_out |>
  filter(!is.na(lter_code)) |>
  group_by(lter_code) |> filter(n() > 1) |>
  ungroup() |> arrange(lter_code) |>
  select(lter_code, usda_code, sciname_auth)
cat("Duplicate lter_codes in main_out:", nrow(lter_code_dup), "\n")
if (nrow(lter_code_dup) > 0) print(lter_code_dup)

# Check: LTER codes with multiple USDA codes
lter_multi_usda <- main_out |>
  filter(!is.na(lter_code), !is.na(usda_code)) |>
  group_by(lter_code) |> filter(n_distinct(usda_code) > 1) |>
  ungroup() |> arrange(lter_code) |>
  select(lter_code, usda_code, sciname_auth)
cat("lter_codes with multiple usda_codes:", n_distinct(lter_multi_usda$lter_code), "\n")
if (nrow(lter_multi_usda) > 0) print(lter_multi_usda)

# Check: USDA codes with multiple LTER codes
usda_multi_lter <- main_out |>
  filter(!is.na(usda_code), !is.na(lter_code)) |>
  group_by(usda_code) |> filter(n_distinct(lter_code) > 1) |>
  ungroup() |> arrange(usda_code) |>
  select(usda_code, lter_code, sciname_auth)
cat("usda_codes with multiple lter_codes:", n_distinct(usda_multi_lter$usda_code), "\n")
if (nrow(usda_multi_lter) > 0) print(usda_multi_lter)

# Check: exact sciname_auth match against USDA Plants
# Accepted taxa: compare against accepted sciname_auth via usda_accepted_lookup
# Synonym taxa: compare against synonym row sciname_auth via usda_syn_lookup
sciname_auth_check <- bind_rows(
  main_out |>
    filter(!usda_code_is_syn) |>
    left_join(usda_accepted_lookup |> select(usda_code_accepted, sciname_auth_usda = sciname_auth),
              by = c("usda_code" = "usda_code_accepted")),
  main_out |>
    filter(usda_code_is_syn) |>
    left_join(usda_syn_lookup |> select(usda_code_syn, sciname_auth_usda = sciname_auth),
              by = c("usda_code" = "usda_code_syn"))
) |>
  mutate(sciname_auth_usda_match = sciname_auth == sciname_auth_usda)

cat("sciname_auth matches USDA Plants exactly:", sum(sciname_auth_check$sciname_auth_usda_match, na.rm = TRUE), "\n")
cat("sciname_auth does not match USDA Plants:", sum(!sciname_auth_check$sciname_auth_usda_match, na.rm = TRUE), "\n")
cat("sciname_auth could not be checked (no USDA entry found):", sum(is.na(sciname_auth_check$sciname_auth_usda_match)), "\n")