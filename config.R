# Ask the user for the root path - usually the Field Crew/IM Sharepoint root
choose_directory = function(caption = 'Select the path to the Field Crew/IM directory ((Documents_Jornada-FC-IM) synced to your local computer. ') {
  if (exists('utils::choose.dir')) {
    utils::choose.dir(caption = caption) 
  } else {
    tcltk::tk_choose.dir(caption = caption)
  }
}

#Set the Root path
root.path <- choose_directory()
# Set other paths
entry.path <- paste(root.path, 'dataentry', sep='/')
im.path <- paste(root.path, 'jornada_im', sep='/')