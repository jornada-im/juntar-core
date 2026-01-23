# publish.R
# Convenience function for publishing a dataset using jerald.

library('jerald')

publish_dataset <- function(id, env, data_path, cred_path, dry_run=TRUE, s3_upload=TRUE){
  # Save the current working directory
  wd <- getwd()
  # Now switch to the target data directory
  setwd(data_path)
  options(scipen=999)   # turns off scientific notation
  # Read your jerald credentials
  source(paste(cred_path, 'jerald_cred.R', sep='/'))
  # Now create or update the dataset on EDI...
  # You must pass `dry.run=FALSE` to really publish the data. Make sure to check
  # dataset identifiers, revision numbers, eml, and other details first.
  result <- tryCatch(
    {
      # Try to publish the dataset uisng the credentials provided
      message('Begin publishing the dataset')
      suppressWarnings(
        publish_dataset_edi(id, mbname, mbcred, edicred, edi.env=env, dry.run = dry_run,
          s3.upload = s3_upload)
        )
      # Return any warnings
    }, warning = function(w){
      message(paste("There was a warning publishing package ", id))
      message("Here's the original warning message:")
      message(conditionMessage(w))
      NULL
      # Return any errors
    }, error = function(e){
      message(paste("There was an error publishing package ", id))
      message("Here's the original error message:")
      message(conditionMessage(e))
      NA
      # Whatever happens, cleanup and return to original working directory
    }, finally = {
      # Clean up
      remove(list=c('mbcred', 'edicred', 'mbname'), envir = .GlobalEnv)
      setwd(wd)
    }
  )
}