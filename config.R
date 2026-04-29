# Root path is set via JORNADA_ROOT in the project .Renviron file.
# Copy .Renviron.example to .Renviron and fill in your local path.
# Or, usethis::edit_r_environ("project") opens it for editing.
root_path <- Sys.getenv("JORNADA_ROOT")
if (root_path == "") {
  stop("JORNADA_ROOT is not set. Add it to the project .Renviron file (see .Renviron.example).")
}

entry_path   <- file.path(root_path, "dataentry")
im_path      <- file.path(root_path, "jornada_im")
#prep_path    <- file.path(root_path, "jornada_im/dataprep/jrn011_npp")
#biomass_path <- file.path(prep_path, "biomass")
#anpp_path    <- file.path(prep_path, "anpp")