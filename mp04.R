 
 #Define the ensure_package function
 ensure_package <- function(pkg) {
   if (!requireNamespace(pkg, quietly = TRUE)) {
     install.packages(pkg)
   }
   library(pkg, character.only = TRUE)
 }
# 
# # Ensure the necessary packages are installed and loaded
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
 ensure_package("sf")

load_county_shapefiles <- function() {
  # Define file and directory paths
  directory <- "data/mp04"
  file_url <- "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_5m.zip"
  zip_path <- file.path(directory, "cb_2023_us_county_5m.zip")
  shapefile_dir <- file.path(directory, "shapefiles")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  # Download the file if it doesn't already exist
  if (!file.exists(zip_path)) {
    download.file(file_url, zip_path, mode = "wb")
    message("File downloaded: ", zip_path)
  } else {
    message("File already exists: ", zip_path)
  }
  
  # Unzip only if the .shp file doesn't already exist
  if (!file.exists(file.path(shapefile_dir, "cb_2023_us_county_5m.shp"))) {
    unzip(zip_path, exdir = shapefile_dir)
    message("Unzipped to: ", shapefile_dir)
  }
  
  # Read the shapefile using sf
  shapefile_path <- file.path(shapefile_dir, "cb_2023_us_county_5m.shp")
  county_shapes <- sf::st_read(shapefile_path, quiet = TRUE)
  
  return(county_shapes)
}

# Call the function
county_shapefiles <- load_county_shapefiles()
county_shapefiles <- county_shapefiles |>
  rename(`County Name`= NAME,
         `Full County Name`= NAMELSAD,
         `State Abbrevation` = STUSPS,
         `State`= STATE_NAME ,
         `Area Description Code`= LSAD ,
         `Area of Land` = ALAND ,
         `Area of Water` = AWATER )

county_shapefiles <- county_shapefiles |>
  select(`County Name`, `Full County Name`, `State Abbrevation`, `State`, `Area Description Code`, `Area of Land`, `Area of Water`) |>
  slice_head(n = 10)

#head(county_shapefiles)
names(county_shapefiles)
View(county_shapefiles)


#Task 2
#US 2024 Presidential Election Results
###########################################################################
scrape_state_results <- function(state_name) {
  # Format the state name into a Wikipedia URL
  base_url <- "https://en.wikipedia.org/wiki"
  state_slug <- gsub(" ", "_", state_name)
  # Handle special cases like Washington
  state_slug <- dplyr::case_when(
    state_name == "Washington" ~ "Washington_(state)",
    TRUE ~ gsub(" ", "_", state_name)
  )
  full_url <- paste0(base_url, "/2024_United_States_presidential_election_in_", state_slug)
  
  # Prepare local HTML cache directory
  html_dir <- "data/mp04/html"
  if (!dir.exists(html_dir)) dir.create(html_dir, recursive = TRUE)
  html_file <- file.path(html_dir, paste0(state_slug, ".html"))
  
  # Download and save HTML only if not already cached
  if (!file.exists(html_file)) {
    resp <- request(full_url) |> req_perform()
    writeBin(resp_body_raw(resp), html_file)
    message("Downloaded: ", state_name)
  } else {
    message("Using cached version for: ", state_name)
  }
  
  # Load HTML
  page <- read_html(html_file)
  
  # Extract all tables
  tables <- page |> html_elements("table")
  
  # Find the table with "County", "Parish", or "Borough" in column headers
  selected_table <- NULL
  for (tbl in tables) {
    headers <- tbl |> html_elements("th") |> html_text(trim = TRUE)
    if (any(grepl("County|Parish|Borough|Area|Region|District", headers, ignore.case = TRUE)))  {
      selected_table <- tbl
      break
    }
  }
  
  # Return NULL if no table found
  if (is.null(selected_table)) {
    warning("No county-level table found for: ", state_name)
    return(NULL)
  }
  
  # Convert to data frame and clean
  df <- selected_table |>
    html_table(fill = TRUE) |>
    janitor::clean_names() |>
    mutate(state = state_name)
 
  # # Standardize column types to avoid bind_rows issues
   df <- df |>
     mutate(across(everything(), as.character))  # Convert all columns to character type 
  # Clean up and filter the columns to ensure only relevant columns
  # df <- df |>
  #   select(county, donald_trump_republican, kamala_harris_democratic, margin, total, state,total_votes_cast) |>
  #   filter(!is.na(county)) # Remove rows without county information
  
  return(df)
}

us_states <- state.name

# Safely attempt scraping for all states
all_election_results <- purrr::map_dfr(us_states, ~{
  tryCatch(scrape_state_results(.x), error = function(e) {
    message("Error scraping ", .x, ": ", e$message)
    return(NULL)
  })
})
# View the first few rows of the combined data frame
head(all_election_results)
sum(!is.null(all_election_results))
names(all_election_results)
View(all_election_results)

cleaned_results <- all_election_results |>
  filter(!is.na(county)) |>
  select(county, donald_trump_republican, kamala_harris_democratic, margin, total, state, various_candidates_other_parties)

cleaned_results <- cleaned_results[-1,] 

#rename selected columns in cleaned_results
US_2024_Election_Results <- cleaned_results |>
  rename(`County Name` = county,
         `Donald Trump (Republican) 2024` = donald_trump_republican,
         `Kamala Harris (Democratic) 2024` = kamala_harris_democratic,
         `Margin 2024` = margin,
         `Total Votes Cast 2024` = total,
         `State` = state,
         `Various Candidates (Other Parties) 2024` = various_candidates_other_parties)
US_2024_Election_Results <- US_2024_Election_Results |>
  select(`County Name`, `Donald Trump (Republican) 2024`, `Kamala Harris (Democratic) 2024`, `Margin 2024`, `Total Votes Cast 2024`, `State`, `Various Candidates (Other Parties) 2024`) |>
  slice_head(n = 10)
View(US_2024_Election_Results)

# # Save to CSV for future use
# write.csv(all_election_results, "data/mp04/2024_presidential_county_results.csv", row.names = FALSE)
# 

#Task 3: Acquire 2020 US Presidential Election Results
#########################################################################################################
scrape_state_results_2020 <- function(state_name) {
  # Format the state name into a Wikipedia URL
  base_url <- "https://en.wikipedia.org/wiki"
  state_slug <- gsub(" ", "_", state_name)
  # Handle special cases like Washington
  state_slug <- dplyr::case_when(
    state_name == "Washington" ~ "Washington_(state)",
    TRUE ~ gsub(" ", "_", state_name)
  )
  full_url <- paste0(base_url, "/2020_United_States_presidential_election_in_", state_slug)
  
  # Prepare local HTML cache directory
  html_dir <- "data/mp04/html_2020"
  if (!dir.exists(html_dir)) dir.create(html_dir, recursive = TRUE)
  html_file <- file.path(html_dir, paste0(state_slug, ".html"))
  
  # Download and save HTML only if not already cached
  if (!file.exists(html_file)) {
    resp <- request(full_url) |> req_perform()
    writeBin(resp_body_raw(resp), html_file)
    message("Downloaded: ", state_name)
  } else {
    message("Using cached version for: ", state_name)
  }
  
  # Load HTML
  page <- read_html(html_file)
  
  # Extract all tables
  tables <- page |> html_elements("table")
  
  # Find the table with "County", "Parish", or "Borough" in column headers
  selected_table <- NULL
  for (tbl in tables) {
    headers <- tbl |> html_elements("th") |> html_text(trim = TRUE)
    if (any(grepl("County|Parish|Borough|Area|Region|District", headers, ignore.case = TRUE)))  {
      selected_table <- tbl
      break
    }
  }
  
  if (is.null(selected_table)) {
    warning("No county-level table found for: ", state_name)
    return(NULL)
  }
  
  # Convert to data frame and clean
  df <- selected_table |>
    html_table(fill = TRUE) |>
    janitor::clean_names() |>
    mutate(state = state_name) |>
    mutate(across(everything(), as.character))
  
  return(df)
}

us_states <- state.name

all_election_results_2020 <- purrr::map_dfr(us_states, ~{
  tryCatch(scrape_state_results_2020(.x), error = function(e) {
    message("Error scraping ", .x, ": ", e$message)
    return(NULL)
  })
})
head(all_election_results_2020)
names(all_election_results_2020)
View(all_election_results_2020)



cleaned_results_2020 <- all_election_results_2020 |>
  filter(!is.na(county)) |>
  select(county, donald_trump_republican, joe_biden_democratic, margin, total, state, various_candidates_other_parties)

cleaned_results_2020 <- cleaned_results_2020[-1,]

#rename selected columns in cleaned_results
US_2020_Election_Results <- cleaned_results_2020 |>
  rename(`County Name` = county,
         `Donald Trump (Republican) 2020` = donald_trump_republican,
         `Joe Biden (Democratic) 2020` = joe_biden_democratic,
         `Margin 2020` = margin,
         `Total Votes Cast 2020` = total,
         `State` = state,
         `Various Candidates (Other Parties) 2020` = various_candidates_other_parties)
US_2020_Election_Results <- US_2020_Election_Results |>
  select(`County Name`, `Donald Trump (Republican) 2020`, `Joe Biden (Democratic) 2020`, `Margin 2020`, `Total Votes Cast 2020`, `State`, `Various Candidates (Other Parties) 2020`) |>
  slice_head(n = 10)
View(US_2020_Election_Results)



# Task 4: Initial Analysis Questions
#Join the dataframes county_shapefiles, US_2024_Election_Results, and US_2020_Election_Results
# to create a single dataframe that contains all the information you need for analysis.
# # Use the `inner_join` function from dplyr to merge the dataframes
names(county_shapefiles)
names(US_2024_Election_Results)
names(US_2020_Election_Results)

merged_data <- US_2024_Election_Results |>
  inner_join(US_2020_Election_Results, by = c("County Name","State")) |>
  inner_join(county_shapefiles, by = c("County Name","State"))
# View the first few rows of the merged data
View(merged_data)
names(merged_data)

# Which county or counties cast the most votes for Trump (in absolute terms) in 2024?

# Step 1: Convert Trump 2024 vote column to numeric
trump_votes <- merged_data |>
  mutate(`Donald Trump (Republican) 2024` = as.numeric(gsub(",", "", `Donald Trump (Republican) 2024`)))

# Step 2: Find the maximum number of Trump votes
top10_trump_votes <- trump_votes |>
  arrange(desc(`Donald Trump (Republican) 2024`)) |>
  slice_head(n = 10) |>
  select(`County Name`, State, `Donald Trump (Republican) 2024`)
  #summarise(max_votes = max(`Donald Trump (Republican) 2024`, na.rm = TRUE))

# # Step 3: Filter rows where Trump got that max number of votes
# largest_trump_votes_2024 <- largest_trump_votes_2024 |>
#   filter(`Donald Trump (Republican) 2024` == max_trump_votes$max_votes) |>
#   select(`County Name`, State, `Donald Trump (Republican) 2024`)

# View the result
View(top10_trump_votes)

#Harris texas, 722695

# Which county or counties cast the most votes for Biden (as a fraction of total votes cast) in 2020?

library(readr)

top_10_biden_fraction <- merged_data |>
  mutate(
    `Biden Votes` = parse_number(`Joe Biden (Democratic) 2020`),
    `Total Votes` = parse_number(`Total Votes Cast 2020`),
    `Fraction of Votes for Biden` = round(`Biden Votes` / `Total Votes`,3)
  ) |>
  filter(!is.na(`Fraction of Votes for Biden`)) |>
  arrange(desc(`Fraction of Votes for Biden`)) |>
  slice_head(n = 10) |>
  select(`County Name`, State, `Biden Votes`, `Total Votes`, `Fraction of Votes for Biden`)

View(top_10_biden_fraction)


# Which county or counties had the largest shift towards Trump (in absolute terms) in 2024?
largest_shift_trump <- merged_data |>
  mutate(
    `Trump Shift` = as.numeric(gsub(",", "", `Donald Trump (Republican) 2024`)) - as.numeric(gsub(",", "", `Donald Trump (Republican) 2020`))
  ) |>
  filter(!is.na(`Trump Shift`)) |>
  arrange(desc(`Trump Shift`)) |>
  slice_head(n = 10) |>
  select(`County Name`, State, `Trump Shift`)
View(largest_shift_trump)

# Which state had the largest shift towards Harris (or smallest shift towards Trump) in 2024? 
# (Note that the total votes for a state can be obtained by summing all counties in that state.)
# library(knitr)
# 

state_trump_shift <- merged_data |>
   mutate(
     Trump_2024 = parse_number(`Donald Trump (Republican) 2024`),
     Trump_2020 = parse_number(`Donald Trump (Republican) 2020`)
   ) |>
   group_by(State) |>
   summarise(
     Total_Trump_2024 = sum(Trump_2024, na.rm = TRUE),
     Total_Trump_2020 = sum(Trump_2020, na.rm = TRUE),
     Trump_Vote_Shift = Total_Trump_2024 - Total_Trump_2020
   ) |>
   arrange(desc(Trump_Vote_Shift))|>  # Smallest shift first
   slice_head(n = 10) 
View(state_trump_shift)


# What is the largest county, by area, in this data set?
largest_county_by_area <- merged_data |>
  mutate(`Area of Land` = as.numeric(`Area of Land`)) |>
  filter(!is.na(`Area of Land`)) |>
  arrange(desc(`Area of Land`)) |>
  slice_head(n = 1) |>
  select(`County Name`, State, `Area of Land`)
largest_county_by_area <- largest_county_by_area |>
  mutate(`Area (sq mi)` = round(`Area of Land` / 2.59e+6, 2))
View(largest_county_by_area)
# Which county has the highest voter density (voters per unit of area) in 2020?

highest_voter_density_2020 <- merged_data |>
  mutate(
    `Total Votes 2020` = parse_number(`Total Votes Cast 2020`),
    `Land Area (sq mi)` = as.numeric(`Area of Land`) / 2.59e+6,  # Convert m² to mi²
    `Voter Density (2020)` = `Total Votes 2020` / `Land Area (sq mi)`
  ) |>
  filter(!is.na(`Voter Density (2020)`), `Land Area (sq mi)` > 0) |>
  arrange(desc(`Voter Density (2020)`)) |>
  slice_head(n = 1) |>
  select(`County Name`, State, `Total Votes 2020`, `Land Area (sq mi)`, `Voter Density (2020)`)
View(highest_voter_density_2020)
# Which county had the largest increase in voter turnout in 2024?

largest_turnout_increase_2024 <- merged_data |>
  mutate(
    `Total Votes 2024` = parse_number(`Total Votes Cast 2024`),
    `Total Votes 2020` = parse_number(`Total Votes Cast 2020`)
  ) |>
  filter(!is.na(`Total Votes 2024`), !is.na(`Total Votes 2020`)) |>
  mutate(
    `Turnout Increase` = `Total Votes 2024` - `Total Votes 2020`
  ) |>
  arrange(desc(`Turnout Increase`)) |>
  slice_head(n = 1) |>
  select(`County Name`, State, `Turnout Increase`)
View(largest_turnout_increase_2024)


#Task 5:
##############################################################################################################################
# Load required packages
# Load required packages
library(dplyr)
library(readr)
library(sf)
library(tigris)
library(ggplot2)
library(units)


shift_data <- merged_data |>
  mutate(
    trump_2020 = parse_number(`Donald Trump (Republican) 2020`),
    trump_2024 = parse_number(`Donald Trump (Republican) 2024`),
    total_2020 = parse_number(`Total Votes Cast 2020`),
    total_2024 = parse_number(`Total Votes Cast 2024`),
    pct_trump_2020 = trump_2020 / total_2020,
    pct_trump_2024 = trump_2024 / total_2024,
    shift = pct_trump_2024 - pct_trump_2020
  )
shift_data <- st_as_sf(shift_data)  # Re-attach geometry and sf methods

reposition_states <- function(geo_data) {
  # Extract geometry
  geo_data$geometry_orig <- st_geometry(geo_data)
  
  # Reposition Hawaii
  geo_data$geometry[geo_data$State == "Hawaii"] <- 
    geo_data$geometry_orig[geo_data$State == "Hawaii"] * 0.35 + c(40, -15)
  
  # Reposition Alaska
  geo_data$geometry[geo_data$State == "Alaska"] <- 
    geo_data$geometry_orig[geo_data$State == "Alaska"] * 0.35 + c(55, -15)
  
  geo_data <- st_as_sf(geo_data)
  geo_data$geometry_orig <- NULL  # Clean up if needed
  return(geo_data)
}
shifted_geometries <- reposition_states(shift_data)

shifted_geometries <- shifted_geometries |>
  mutate(centroid = st_centroid(geometry)) |>
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

arrow_data <- shifted_geometries |>
  mutate(
    dx = shift * 5,  # scale factor; adjust as needed
    dy = 0,
    xend = lon + dx,
    yend = lat + dy
  )
# Add dx and dy based on shift
arrow_data <- shifted_geometries |>
  filter(!is.na(lon), !is.na(lat), !is.na(shift)) |>
  mutate(
    dx = shift * 5,  # adjust scaling factor as needed for visibility
    dy = 0  # arrows go horizontally (right = red shift, left = blue shift)
  )

ggplot() +
  geom_sf(data = shifted_geometries, fill = "gray95", color = "white", size = 0.1) +
  geom_segment(
    data = arrow_data,
    aes(x = lon, y = lat, xend = lon + dx, yend = lat + dy, color = shift),
    arrow = arrow(length = unit(0.1, "inches")),
    linewidth = 0.3
  ) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(
    title = "Shift in Republican Vote Share (2020 → 2024)",
    subtitle = "Arrows point rightward for increased Trump support, leftward for decreased"
  )











