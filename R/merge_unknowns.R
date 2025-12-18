library('tidyverse')

# Function to find and return rows of a dataframe that have
# duplicate values in a column (LTER_code)
find_dups <- function(df, varname){
  n_occur <- data.frame(table(df[varname]))
  print(n_occur[n_occur$Freq > 1,])
  dups <- df[df[[varname]] %in% n_occur$Var1[n_occur$Freq > 1],]
  return(dups)
  }
# Load crossref and check for duplicates
crossref <- read_csv("LTER_to_USDA_PLANTS_codes.csv")
find_dups(crossref,'LTER_code')
# Load the NPP-specific crossref and check for duplicates
crossref_NPP <- read_csv("LTER_to_USDA_PLANTS_codes_NPP.csv")
find_dups(crossref_NPP,'LTER_code')

# Merge into one dataframe and find duplicates
crossref_merge <- merge(crossref, crossref_NPP,
                        by=c('LTER_code','USDA_code','Species_binomial',
                             'habit','form','cpath', 'comment'),all=TRUE)
dups <- find_dups(crossref_merge, 'LTER_code')

# Loop through each duplicate and decide which to remove
varname = 'LTER_code'
crossref_merge['remove'] <- FALSE
for(d in unique(dups[[varname]])) {
  test <- (crossref_merge[[varname]] == d)
  indices <- which(test)
  subset <- crossref_merge[indices,]
  print(subset)
  n <- readline("Which index to remove?: ")
  crossref_merge$remove[as.numeric(n)] <- TRUE
  
}
# Remove the duplicates
crossref_new <- crossref_merge[!crossref_merge$remove,]
crossref_new <- crossref_new %>% select(-LTER_code_consistent, -remove)

# Write de-duplicated codes to csv
write_csv(crossref_new, './LTER_to_USDA_PLANTS_codes.csv')
