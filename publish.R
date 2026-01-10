# build_eml.210011004.R
# 
# BOILERPLATE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This is a template build script using R to prepare eml and 
# send a dataset to EDI using the jerald R package. You need 
# credentials for this to work, and there is a template credentials
# file in jrn-metabase-utils repository.
#
# All metadata documents (abstract, methods) and any data entity
# files (CSVs, images, zipfiles etc.) must be in the directory with 
# this script. The data entities, abstract, and methods files
# should be named to match the values in the lter-metabase 
# (DataSetEntities.FileName, DataSet.Abstract and 
# DataSetMethod.Description).
#
# You can safely remove this and other boilerplate and use
# the rest to design a new R script for your dataset.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

library('jerald')


publish_package <- function(id, env, data_path, cred_path, dry_run=TRUE, s3_upload=TRUE){
  wd <- getwd()
  setwd(data_path)
  options(scipen=999)   # turns off scientific notation
  source(paste(cred_path, 'jerald_cred.R', sep='/'))
  # Now create or update the dataset on EDI...
  # # You must pass `dry.run=FALSE` to really publish the data. Make sure to check
  # # dataset identifiers, revision numbers, eml, and other details first.
  publish_dataset_edi(id, mbname, mbcred, edicred, edi.env=env, dry.run = dry_run,
    s3.upload = s3_upload)
  # Clean up
  remove(mbcred, edicred, mbname, mbcred_path, destcred_path)
  setwd(wd)
}