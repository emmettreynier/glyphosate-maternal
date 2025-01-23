# Load required packages
library(pacman)
p_load(utils, here, stringr)

# Define the parent directory containing the 20 folders
parent_dir = here('data/health-restricted/data-nchs')
# Define the destination directory for the extracted .txt files
dest_dir = here('data/health-restricted/raw')
# Create the "raw" directory if it doesn't exist
if (!dir.exists(dest_dir)) dir.create(dest_dir)
# Get a list of all subdirectories in the parent directory
folders = list.dirs(parent_dir, recursive = FALSE) |>
  str_subset('NatAC\\d{4}$')
# Get folders already run
folders_run = list.files(dest_dir) |> str_remove('\\.txt')
# Loop through each folder
for (folder in folders[!(basename(folders) %in% folders_run)]) {
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
  txt_file = list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(txt_file) > 1) {
    txt_file = txt_file[str_detect(basename(txt_file), 'US|us')]
    if (length(txt_file) > 1) stop(paste0('more than 1 txt file in', folder))
  }
  if (length(txt_file) == 0) {
    print(paste0('No txt file in', folder))
    next
  }
  # Copying into raw directory
  file.copy(
    txt_file, 
    file.path(dest_dir, paste0(basename(folder), '.txt')), 
    overwrite = TRUE
  )
  # Deleting files from tempdir
  unlink(
    list.files(temp_dir, full.names = TRUE) |> str_subset('\\.txt|\\.pdf|\\.doc|\\.TXT'), 
    recursive = TRUE
  )
}
