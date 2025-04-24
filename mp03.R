
# Define the ensure_package function
ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Ensure the necessary packages are installed and loaded
ensure_package("dplyr")
ensure_package("stringr")
ensure_package("tidyr")
ensure_package("httr2")
ensure_package("rvest")
ensure_package("datasets")
ensure_package("purrr")
ensure_package("DT")
ensure_package("jsonlite")
ensure_package("httr")

load_songs <- function() {
  # Define file and directory paths
  directory <- "data/mp03"
  file_url <- "https://raw.githubusercontent.com/gabminamedez/spotify-data/refs/heads/master/data.csv"
  file_path <- file.path(directory, "data.csv")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  # Download the file if it doesn't already exist
  if (!file.exists(file_path)) {
    download.file(file_url, file_path)
    message("File downloaded: ", file_path)
  } else {
    message("File already exists: ", file_path)
  }
  
  # Read the CSV file into a data frame
  song_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Process the artists column
  clean_artist_string <- function(x){
    str_replace_all(x, "\\['", "") |> 
      str_replace_all("'\\]", "") |>
      str_replace_all(" '", "")
  }
  
  # Process and return the data
  processed_data <- song_data |> 
    separate_longer_delim(artists, ",") |>
    mutate(artist = clean_artist_string(artists)) |>
    select(-artists)
  
  return(processed_data)
}

# Now actually call the function and save the result
song_data <- load_songs()

# View the first few rows to confirm it worked
head(song_data)
names(song_data)

load_playlist <- function() {
  p_directory <- "data/mp03/playlists"
  p_url <- "https://raw.githubusercontent.com/DevinOgrady/spotify_million_playlist_dataset/master/data1/"
  j_file_name <- "mpd.slice.0-999.json"
  file_path_1 <- file.path(p_directory, j_file_name)
  file_url_1 <- paste0(p_url, j_file_name)
  
  if (!dir.exists(p_directory)) {
    dir.create(p_directory, recursive = TRUE)
  }
  
  if (!file.exists(file_path_1)) {
    download.file(file_url_1, destfile = file_path_1, method = "auto")
  }
  
  playlist_data <- fromJSON(file_path_1, simplifyVector = FALSE)  
  
  if (!"playlists" %in% names(playlist_data)) return(NULL)
  
  playlist_tracks <- map_dfr(playlist_data$playlists, function(pl) {
    if (!is.list(pl)) return(NULL)
    if (!("tracks" %in% names(pl))) return(NULL)
    if (!is.list(pl$tracks) || length(pl$tracks) == 0) return(NULL)
    
    tidy_data <- tryCatch(
      bind_rows(pl$tracks),
      error = function(e) NULL
    )
    
    if (is.null(tidy_data) || nrow(tidy_data) == 0) return(NULL)
    
    tidy_data |>
      mutate(
        playlist_name      = pl$name,
        playlist_id        = pl$pid,
        playlist_followers = pl$num_followers,
        playlist_position  = row_number()
      ) |>
      select(
        playlist_name,
        playlist_id,
        playlist_followers,
        playlist_position,
        track_name,
        track_uri,
        artist_name,
        artist_uri,
        album_name,
        album_uri,
        duration_ms
      ) |>
      rename(
        track_id  = track_uri,
        artist_id = artist_uri,
        album_id  = album_uri,
        duration  = duration_ms
      )
  })
  
  return(playlist_tracks)
}

# Load playlists data
playlists_data <- load_playlist()
glimpse(playlists_data)
head(playlists_data)
View(playlists_data)

#‘Rectangle’ the Playlist Data
strip_spotify_prefix <- function(x){
  library(stringr)
  str_extract(x, ".*:.*:(.*)", group=1)
}

playlists_data <- playlists_data |>
  mutate(
    track_id  = sapply(track_id, strip_spotify_prefix),
    artist_id = sapply(artist_id, strip_spotify_prefix),
    album_id  = sapply(album_id, strip_spotify_prefix)
  )
View(playlists_data)


#create a kabble of the first 10 rows of the playlist data
kable(playlists_data[1:10,])


############################
#Task 4: Initial Exploration
#How many distinct tracks and artists are represented in the playlist data?

library(dplyr)
library(knitr)
library(kableExtra)
#Count the number of distinct artists and tracks
distinct_artists <- playlists_data |>
  distinct(artist_id) |>
  summarise("Number of Artists" = n())
#print(distinct_artists)

#create a kable of distinct artists
kable(distinct_artists)


#Count the number of distinct tracks
distinct_tracks <- playlists_data |>
  distinct(track_id) |>
  summarise("Number of Tracks" = n())
#print(distinct_tracks)

#create a kable of distinct tracks
kable(distinct_tracks)

# Find the most popular tracks
most_popular_tracks <- playlists_data |>
  group_by(track_name) |>
  summarise(`Number of Appearances` = n()) |>
  arrange(desc(`Number of Appearances`)) |>
  slice(1:5) |>
  rename("Name of Track" = track_name) 
#print(most_popular_tracks)
#View(most_popular_tracks)

# Create a kable of the most popular tracks
kable(most_popular_tracks)


#join data frames by track id column
song_data <- song_data |>
  rename("track_id" = id)

#Find the most popular track that has no entry in song  data
no_match_tracks <- playlists_data |>
  anti_join(song_data, by = "track_id") 

most_pop_nochar <- (
  no_match_tracks |>
    count(`Name of Track`= track_name, `Artist` = artist_name, sort = TRUE) |>
    slice_max(n, n = 1) |>
    rename(`Number of Appearances` = n)
)

#View(most_pop_nochar)

# Create a kable of the most popular track without characteristics
kable(most_pop_nochar)


# use left join to combine the song data with the playlist data
join_data <- song_data |>
  left_join(playlists_data, by = "track_id")

# Find the most danceable track
most_danceable_track <- join_data |>
  select(track_name, artist_name, danceability, playlist_name) |>
  arrange(desc(danceability)) |>
  rename("Name of Track" = track_name, "Artist" = artist_name, "Danceability" = danceability, "Playlist Name" = playlist_name) |>
  slice(1)

#View(most_danceable_track)

# Create a kable table to display the most danceable track
kable(most_danceable_track)


# Find the playlist with the longest average track length by grouping by playlist name 
# and calculating the average track length (average of the duration column).
# Then, calculate the average length in minutes and sort the data frame in descending order.
#names(playlists_data)
longest_playlist <- playlists_data |>
  group_by(playlist_name) |>
  summarise(avg_length = mean(duration)) |>
  mutate(avg_length_min = round(avg_length/60000, 0)) |>
  arrange(desc(avg_length)) |>
  rename("Playlist Name" = playlist_name, "Average Track Length (ms)" = avg_length, "Average Track Length (min)" = avg_length_min) |>
  slice(1)
#View(longest_playlist)

# Create a kable Extra table to display the playlist with the longest average track length
kable(longest_playlist)


#Find the most popular playlist by grouping by playlist id and name and number of followers
most_popular_playlist <- playlists_data |>
  distinct(playlist_id, playlist_name, playlist_followers) |>
  slice_max(playlist_followers) |>
  rename("Playlist ID" = playlist_id,"Playlist Name" = playlist_name, "Number of Followers" = playlist_followers) 
#View(most_popular_playlist)

# Create a kable table to display the most popular playlist

kable(most_popular_playlist)


#use inner join to combine the song data with the playlist data
inner_join_data <- song_data |>
  inner_join(playlists_data, by = "track_id")

#using the inner join data find the correlation between the popularity and the number of playlist appearances
pop_correlation <- inner_join_data |>
  group_by(track_id, track_name) |>
  summarise(num_of_playlists = n(),
            mean_popularity = mean(popularity),
            .groups = "drop") 

#create a new column to categorize the popularity into groups (Low, Medium, High)

pop_correlation <- pop_correlation |>
  mutate(popularity_group = cut(
    mean_popularity,
    breaks = c(0, 25, 75, 100),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE
  ))
#View(pop_correlation)

# Create a boxplot to display the popularity groups and their average playlist appearances
library(ggplot2)
ggplot(pop_correlation, aes(x = popularity_group, y = num_of_playlists)) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "blue") +
  labs(
    title = "Playlist Appearances by Popularity Group",
    x = "Popularity Group",
    y = "Number of Playlist Appearances"
  ) +
  theme_minimal()



#Using the inner join data and mean, we will filter the data to find the most popular songs released in a distinct year.
pop_year <- inner_join_data |>
  group_by(year) |>
  summarise(mean_pop_songs = mean(popularity, na.rm = TRUE)) |>
  arrange(desc(mean_pop_songs))

#head(pop_year)
top_ten_pop_year <- pop_year |>
  slice_head(n=10)
#create kable of the most popular songs released in a distinct year
kable(top_ten_pop_year) 
#View(pop_year)

#visualize the average popularity of songs by year
pop_year_plot <- ggplot(pop_year, aes(x = year, y = mean_pop_songs)) +
  geom_line(color = "blue") +
  geom_point(color = "purple") +
  labs(title = " Average Popularity of Released Songs by Year",
       x = "Year",
       y = "Average Popularity")
print(pop_year_plot)




# Using the inner join data, we will filter the data to find the year with the highest average danceability.
danceability_year <- inner_join_data |>
  group_by(year) |>
  summarise(mean_danceability = mean(danceability, na.rm = TRUE)) |>
  slice_max(mean_danceability, n = 1) |>
  rename("Average Danceability" = mean_danceability)

#View(danceability_year)

#create a kable of the year with the highest average danceability

kable(danceability_year)

#data to view all average danceability by year
danceability_year_all <- inner_join_data |>
  group_by(year) |>
  summarise(mean_danceability = mean(danceability, na.rm = TRUE)) |>
  arrange(desc(mean_danceability)) 

#visualize the average danceability of songs by year using a line plot
danceability_year_plot <- ggplot(danceability_year_all, aes(x = year, y = mean_danceability)) +
  geom_line(color = "deepskyblue") +
  geom_point(color = "black") +
  labs(title = "Average Danceability by Year",
       x = "Year",
       y = "Average Danceability")
print(danceability_year_plot)




#Mutate a column to calculate the decade using integer division
pop_decade <- inner_join_data |>
  mutate(decade = (year %/% 10) * 10) |>
  group_by(decade) |>
  summarise(num_songs = n()) |>
  arrange(desc(num_songs)) |>
  rename("Number of Songs" = num_songs, "Decade" = decade)

#View(pop_decade)
# Create a kable Extra table to display the number of songs by decade
kable(pop_decade)

# Create a bar plot to visualize the number of songs by decade
pop_decade_plot <- ggplot(pop_decade, aes(x = factor(`Decade`), y = `Number of Songs`)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  labs(title = "Number of Songs by Decade",
       x = "Decade",
       y = "Number of Songs") +
  theme_minimal()

print(pop_decade_plot)

# Calculate the frequency of each key
key_frequency <- inner_join_data |>
  group_by(key) |>
  summarize(count = n()) |>
  arrange(desc(count))

# Create a polar plot...
ggplot(key_frequency, aes(x = as.factor(key), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  coord_polar(start = 0) +
  theme_bw() +
  labs(title = "Frequency of Musical Keys",
       x = "Key",
       y = "Count of Keys") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16))


#using the inner join data, we will filter the data to find the most popular track lengths (in minutes)
inner_join_data<- inner_join_data |>
  mutate(track_length = duration/ 60000)

#find the mean, median, min, and max track lengths
track_length_data <- inner_join_data |>
  summarise(
    avg_length = mean(track_length, na.rm = TRUE),
    median_length = median(track_length, na.rm = TRUE),
    min_length = min(track_length, na.rm = TRUE),
    max_length = max(track_length, na.rm = TRUE)
  ) 
#View(track_length_data)
#Create a kable table to display the track length data
kable(track_length_data)

#Create a histogram to visualize the distribution of track lengths
ggplot(inner_join_data, aes(x = track_length)) +
  geom_histogram(binwidth = 0.6, fill = "cornflowerblue", color = "black") +
  labs(title = "Most Popular Track Lengths",
       x = "Track Length (minutes)",
       y = "Number of Songs") +
  theme_bw() +
  scale_x_continuous(
    limits = c(0, 10),       # Only min and max go here
    breaks = seq(0, 10, 2)   # Add ticks every 2 minutes
  )

#Creating the Ultimate Playlist

#Using inner join data we will filter the data to find the anchor songs and their characteristics. 
anchor_songs <- inner_join_data |>
  filter(track_name %in% c("Wake Me Up When September Ends", "Fix You")) |> 
  distinct(track_id, track_name, artist_name, key, tempo, year, popularity, acousticness, danceability, valence, instrumentalness,energy)
#View(anchor_songs)

#distinct(inner_join_data, track_name) |> View()

#Heuristic 1: Songs that Commonly Appear with the Anchor Song
#We'll find songs that appeared in the same playlists as the anchor song(s).

anchor_ids <- anchor_songs$track_id

co_occurring_tracks <- playlists_data |>
  filter(track_id %in% anchor_ids) |>
  select(playlist_id) |>
  inner_join(playlists_data, by = "playlist_id") |>
  filter(!track_id %in% anchor_ids) |>  # exclude the anchor itself
  count(track_name, artist_name, sort = TRUE) |>
  slice_max(n, n = 20)
# Rename columns for clarity

co_occurring_tracks <- co_occurring_tracks |>
  rename(
    `Track Name` = track_name,
    `Artist Name` = artist_name,
    `Number of Playlists` = n
  )

#View(co_occurring_tracks)
top_ten_co_ocurring <- co_occurring_tracks |>
  slice_head(n = 10)
# Create a kable table to display the similar co-occuring tracks
kable(top_ten_co_ocurring) 

################
#Heuristic 2: Same Key + Similar Tempo (±5 BPM)
library(purrr)
similar_key_tempo <- inner_join_data |>
  filter(
    key %in% anchor_songs$key,
    map_lgl(tempo, ~ any(abs(.x - anchor_songs$tempo) <= 5))
  ) |>
  distinct(track_id, track_name, artist_name, tempo, key)|>
  slice_max(tempo, n = 15) # Get top 10 similar tracks

# Rename columns for clarity
similar_key_tempo <- similar_key_tempo |>
  rename(
    `Track Name` = track_name,
    `Artist Name` = artist_name,
    `Tempo (BPM)` = tempo,
    `Key` = key
  )

#View(similar_key_tempo)
top_ten_keytemp <- similar_key_tempo  |>
  slice_head(n = 10)
# Create a kable table to display the similar key tempo tracks
kable(top_ten_keytemp) 


#Heuristic 3: Same Artist
same_artist_tracks <- inner_join_data |>
  filter(artist_name %in% anchor_songs$artist_name) |>
  distinct(track_id, track_name, artist_name)

#View(same_artist_tracks)
# Create a kable Extra table to display the same artist tracks

top_ten_same <- same_artist_tracks |>
  slice_head(n = 10)

kable(top_ten_same) 


# heuristic 4 - 
# We'll allow a range (±0.1) around the acousticness/danceability of the anchor songs.
#find the range of years for the anchor songs
#and filter the data to find the similar songs


anchor_range <- anchor_songs |>
  summarise(
    min_acoustic = min(acousticness) - 0.1,
    max_acoustic = max(acousticness) + 0.1,
    min_dance = min(danceability) - 0.1,
    max_dance = max(danceability) + 0.1,
    year_range = list(unique(year))  # make year_range a list-column
  )

# Pull the scalar values from the 1-row data frame
min_acoustic <- anchor_range$min_acoustic
max_acoustic <- anchor_range$max_acoustic
min_dance <- anchor_range$min_dance
max_dance <- anchor_range$max_dance
year_range <- anchor_range$year_range[[1]]  # unlist the year_range safely

# Now filter safely
similar_vibe_tracks <- inner_join_data |>
  filter(
    year %in% year_range,
    acousticness >= min_acoustic,
    acousticness <= max_acoustic,
    danceability >= min_dance,
    danceability <= max_dance
  ) |>
  distinct(track_id, track_name, artist_name, year)


#View(similar_vibe_tracks)
top_ten_vibe <- similar_vibe_tracks |>
  slice_head(n = 10)

#create a kable table to display the similar vibe tracks
kable(top_ten_vibe)

#Valence(Mood) or emotional tone of the track
# names(anchor_songs)
#We will use the valence column to find similar songs
valence_range <- anchor_songs |> summarise(
  min_valence = min(valence) - 0.001,
  max_valence = max(valence) + 0.001
)
#compare  the valence of the anchor songs to the valence of the other songs
similar_valence_tracks <- inner_join_data |>
  filter(
    valence >= valence_range$min_valence,
    valence <= valence_range$max_valence
  ) |>
  distinct(track_id, track_name, artist_name, valence)

# Rename columns for clarity
similar_valence_tracks <- similar_valence_tracks |>
  rename(
    `Track Name` = track_name,
    `Artist Name` = artist_name,
    `Valence` = valence
  )

#View(similar_valence_tracks)
top_ten_valence <- similar_valence_tracks |>
  slice_head(n = 10)
# Create a kable table to display the similar valence tracks
kable(top_ten_valence) 



#final Playlist Candidate
#Now combine all those results and filter down to at least 20 unique songs, making sure at least 8 are not “popular” (say, popularity < 60)
anchor_artists <- unique(anchor_songs$artist_name)
anchor_ids <- unique(anchor_songs$track_id)
all_candidates <- bind_rows(
  co_occurring_tracks,
  similar_key_tempo,
  similar_vibe_tracks,
  similar_valence_tracks
) |>
  filter(!artist_name %in% anchor_artists) |>
  distinct(track_id, track_name, artist_name)

# Join with popularity info
final_candidates <- all_candidates |>
  inner_join(inner_join_data |> select(track_name, artist_name, popularity,valence,energy,danceability), by = c("track_name", "artist_name")) |>
  distinct() |>
  mutate(
    is_popular = ifelse(popularity >= 60, "Popular", "Not Popular"))
popular_split <- final_candidates |>
  group_split(is_popular)

non_popular <- popular_split[[which(levels(as.factor(final_candidates$is_popular)) == "Not Popular")]] |>
  slice_head(n = 4)

popular <- popular_split[[which(levels(as.factor(final_candidates$is_popular)) == "Popular")]] |>
  slice_head(n = 8)

final_playlist <- bind_rows(non_popular, popular)

# Select relevant columns from anchor_songs
anchor_clean <- anchor_songs |>
  select(track_id, track_name, artist_name, popularity, valence, energy, danceability)

# Combine with the rest of the playlist
final_playlist <- bind_rows(final_playlist, anchor_clean) |>
  distinct(track_id, .keep_all = TRUE) # Avoid duplicates

#View(final_playlist)


# View top 25 for now
#View(final_candidates |> slice(1:25))
#View(similar_valence_tracks)

###########################
#colnames(inner_join_data)

# Order by sonic flow
ordered_playlist <- final_playlist |>
  arrange(valence + energy)

# View final playlist
#View(ordered_playlist)

# Visualize Playlist
library(ggplot2)

ordered_playlist <- ordered_playlist |>
  mutate(
    energy = as.numeric(energy),
    valence = as.numeric(valence),
    danceability = as.numeric(danceability)
  )

# Ensure the 'ordered_playlist' has all the necessary columns
ordered_playlist <- ordered_playlist |>
  mutate(order = row_number())

# Create the plot
# Optional: Ensure track_name is a factor to maintain order
ordered_playlist <- ordered_playlist |>
  mutate(track_name = factor(track_name, levels = track_name))

# Now build the plot
ggplot(ordered_playlist, aes(x = track_name)) +
  geom_line(aes(y = energy), color = "orange", size = 1.2, group = 1) +
  geom_line(aes(y = danceability), color = "steelblue", size = 1.2, group = 1) +
  geom_line(aes(y = valence), color = "purple", size = 1.2, group = 1) +
  labs(
    title = "Ultimate Playlist: Echoes of The Boulevard",
    subtitle = "Energy (orange), Danceability (blue), Valence (purple)",
    x = "Track Name",
    y = "Feature Value (0–1 scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(face = "bold")
  )


