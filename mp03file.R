load_songs <- function() {
  # Load necessary libraries
  library(tidyr)
  library(stringr)
  library(dplyr)
  
  # Define the directory and file paths
  dir_path <- "data/mp03"
  file_name <- "data.csv"
  file_path <- file.path(dir_path, file_name)
  
  # Create the directory if it doesn't exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Download the file if it doesn't already exist
  if (!file.exists(file_path)) {
    url <- "https://raw.githubusercontent.com/gabminamedez/spotify-data/refs/heads/master/data.csv"
    download.file(url, destfile = file_path, method = "auto")
  }
  
  # Read the CSV file
  songs_df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Helper function to clean artist strings
  clean_artist_string <- function(x) {
    str_replace_all(x, "\\['", "") |>
      str_replace_all("'\\]", "") |>
      str_replace_all("'", "") |>
      str_trim()
  }
  
  # Tidy up the artists column: separate multiple artists into separate rows and clean the text
  songs_cleaned <- songs_df |>
    separate_longer_delim(artists, delim = ",") |>
    mutate(artist = clean_artist_string(artists)) |>
    select(-artists)
  
  return(songs_cleaned)
}

# Example usage
songs <- load_songs()
head(songs)
# 
# load_playlists <- function() {
#   library(jsonlite)
#   library(httr)
#   
#   # Define base path and GitHub raw URL
#   base_dir <- "data/mp03/playlists"
#   repo_url <- "https://raw.githubusercontent.com/DevinOgrady/spotify_million_playlist_dataset/master/data1/"
#   
#   # Create directory if it doesn't exist
#   if (!dir.exists(base_dir)) {
#     dir.create(base_dir, recursive = TRUE)
#   }
#   
#   # List of file names to download (1000 files named mpd.slice.NNNN.json)
#   file_indices <- 0:999
#   file_names <- sprintf("mpd.slice.%04d.json", file_indices)
#   
#   # Download missing files only
#   for (file in file_names) {
#     local_path <- file.path(base_dir, file)
#     if (!file.exists(local_path)) {
#       message("Downloading: ", file)
#       file_url <- paste0(repo_url, file)
#       tryCatch({
#         download.file(file_url, destfile = local_path, mode = "wb")
#       }, error = function(e) {
#         warning("Failed to download ", file)
#       })
#     }
#   }
#   
#   # Read all JSON files into a list
#   json_files <- list.files(base_dir, pattern = "\\.json$", full.names = TRUE)
#   playlists <- lapply(json_files, function(path) {
#     tryCatch({
#       fromJSON(path)
#     }, error = function(e) {
#       warning("Failed to read ", path)
#       NULL
#     })
#   })
#   
#   # Remove NULLs in case of any read failures
#   playlists <- Filter(Negate(is.null), playlists)
#   
#   return(playlists)
# }
# 
# load_playlists <- function() {
library(jsonlite)
library(httr)

# Define base path and GitHub raw URL
base_dir <- "data/mp03/playlists"
repo_url <- "https://raw.githubusercontent.com/DevinOgrady/spotify_million_playlist_dataset/master/data1/"

# Create directory if it doesn't exist
if (!dir.exists(base_dir)) {
  dir.create(base_dir, recursive = TRUE)
}

# Generate list of file names (mpd.slice.0000.json to mpd.slice.0999.json)
file_indices <- 0:999
file_names <- sprintf("mpd.slice.%04d.json", file_indices)

# Loop through and download files if they don't already exist
for (file in file_names) {
  local_path <- file.path(base_dir, file)
  
  if (!file.exists(local_path)) {
    message("Downloading: ", file)
    file_url <- paste0(repo_url, file)
    tryCatch({
      download.file(file_url, destfile = local_path, mode = "wb")
    }, error = function(e) {
      warning("Failed to download ", file)
    })
  } else {
    message("File already exists: ", file)
  }
}

# Read all JSON files into a list
json_files <- list.files(base_dir, pattern = "\\.json$", full.names = TRUE)
playlists <- lapply(json_files, function(path) {
  tryCatch({
    fromJSON(path)
  }, error = function(e) {
    warning("Failed to read ", path)
    NULL
  })
})

# Remove any NULLs (files that failed to parse)
playlists <- Filter(Negate(is.null), playlists)

return(playlists)
}

strip_spotify_prefix <- function(x) {
  stringr::str_extract(x, ".*:.*:(.*)")
}
rectangle_playlists <- function(playlists) {
  library(purrr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  # Helper to extract tracks from a single playlist object
  extract_tracks <- function(playlist_obj) {
    map_dfr(playlist_obj$playlists, function(p) {
      if (length(p$tracks) == 0) return(NULL)
      tibble(
        playlist_name = p$name,
        playlist_id = p$pid,
        playlist_followers = p$num_followers,
        playlist_position = map_int(p$tracks, "pos"),
        artist_name = map_chr(p$tracks, ~ .x$artist_name),
        artist_id = map_chr(p$tracks, ~ .x$artist_uri),
        track_name = map_chr(p$tracks, ~ .x$track_name),
        track_id = map_chr(p$tracks, ~ .x$track_uri),
        album_name = map_chr(p$tracks, ~ .x$album_name),
        album_id = map_chr(p$tracks, ~ .x$album_uri),
        duration = map_int(p$tracks, ~ .x$duration_ms)
      )
    })
  }
  
  # Apply over all playlists
  flat_data <- map_dfr(playlists, extract_tracks)
  
  # Clean IDs
  flat_data <- flat_data %>%
    mutate(
      artist_id = strip_spotify_prefix(artist_id),
      track_id = strip_spotify_prefix(track_id),
      album_id = strip_spotify_prefix(album_id)
    )
  
  return(flat_data)
}

