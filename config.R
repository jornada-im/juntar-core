# Ask the user for the root path - usually the Field Crew/IM Sharepoint root
choose_directory = function(caption = "Select the path to the Field Crew/IM directory (Documents_Jornada-FC-IM) synced to your local computer. ") {
  message(caption)
  env <- Sys.getenv()
  # If on MS Windows use this
  if (exists("utils::choose.dir")) { 
    utils::choose.dir(caption = caption, default = env[["HOME"]])
  # Linux and MacOS use this
  } else {
    tcltk::tk_choose.dir(caption = caption, default = env[["HOME"]])
  }
}

#Set the Root path
root_path <- choose_directory()
# Set other paths
entry_path <- paste(root_path, "dataentry", sep="/")
im_path <- paste(root_path, "jornada_im", sep="/")