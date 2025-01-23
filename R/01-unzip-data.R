# Load required packages
library(pacman)
p_load(utils, here, stringr)

# Define the parent directory containing the 20 folders
parent_directory = here('data/health-restricted/data-nchs')
# Define the destination directory for the extracted .txt files
destination_directory = here('data/health-restricted/raw')
# Create the "raw" directory if it doesn't exist
if (!dir.exists(destination_directory)) {
  dir.create(destination_directory)
}

# Get a list of all subdirectories in the parent directory
folders = list.dirs(parent_directory, recursive = FALSE) |>
  str_subset('^NatAC\\d{4}$')

# Loop through each folder
for (folder in folders) {
  # Find the zip file in the current folder
  zip_file = list.files(folder, pattern = "\\.zip$", full.names = TRUE)
  # Skip the folder if no zip file is found
  if (length(zip_file) == 0) next
  # Ensure there is only one zip file per folder
  if (length(zip_file) > 1) stop("More than one zip file")
  # Create a temporary directory to extract the zip file
  temp_dir = tempdir()
  unzip(zip_file, exdir = temp_dir)
  # Find the .txt file in the extracted contents
  txt_file = list.files(temp_dir, pattern = "\\.txt$")
  if (length(txt_file) == 0) next
  if (length(txt_file) > 1) stop("More than one txt file")
  # Copying into raw directory
  file.copy(
    txt_file, 
    file.path(destination_directory, paste0(basename(folder), '.txt')), 
    overwrite = TRUE
  )
}