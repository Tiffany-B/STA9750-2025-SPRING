
#The following Code loads the required libraries and packages
ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE))
}

ensure_package(dplyr)
ensure_package(httr2)
ensure_package(rvest)
ensure_package(datasets)
ensure_package(purrr)
ensure_package(DT)
ensure_package(stringr)

#The following code loads the required data
get_eia_sep <- function(state, abbr){
  state_formatted <- str_to_lower(state) |> str_replace_all("\\s", "")
  
  dir_name <- file.path("data", "mp02")
  file_name <- file.path(dir_name, state_formatted)
  
  dir.create(dir_name, showWarnings=FALSE, recursive=TRUE)
  
  if(!file.exists(file_name)){
    BASE_URL <- "https://www.eia.gov"
    REQUEST <- request(BASE_URL) |> 
      req_url_path("electricity", "state", state_formatted)
    
    RESPONSE <- req_perform(REQUEST)
    
    resp_check_status(RESPONSE)
    
    writeLines(resp_body_string(RESPONSE), file_name)
  }
  
  TABLE <- read_html(file_name) |> 
    html_element("table") |> 
    html_table() |>
    mutate(Item = str_to_lower(Item))
  
  if("U.S. rank" %in% colnames(TABLE)){
    TABLE <- TABLE |> rename(Rank = `U.S. rank`)
  }
  
  CO2_MWh <- TABLE |> 
    filter(Item == "carbon dioxide (lbs/mwh)") |>
    pull(Value) |> 
    str_replace_all(",", "") |>
    as.numeric()
  
  PRIMARY <- TABLE |> 
    filter(Item == "primary energy source") |> 
    pull(Rank)
  
  RATE <- TABLE |>
    filter(Item == "average retail price (cents/kwh)") |>
    pull(Value) |>
    as.numeric()
  
  GENERATION_MWh <- TABLE |>
    filter(Item == "net generation (megawatthours)") |>
    pull(Value) |>
    str_replace_all(",", "") |>
    as.numeric()
  
  data.frame(CO2_MWh               = CO2_MWh, 
             primary_source        = PRIMARY,
             electricity_price_MWh = RATE * 10, # / 100 cents to dollars &
             # * 1000 kWh to MWH 
             generation_MWh        = GENERATION_MWh, 
             state                 = state, 
             abbreviation          = abbr
  )
}

EIA_SEP_REPORT <- map2(state.name, state.abb, get_eia_sep) |> list_rbind()

ensure_package(scales)
ensure_package(DT)

#The following code cleans the data and answers the questions
EIA_SEP_REPORT <- EIA_SEP_REPORT |> 
  select(-abbreviation) |>
  arrange(desc(CO2_MWh)) |>
  mutate(CO2_MWh = number(CO2_MWh, big.mark=","), 
         electricity_price_MWh = dollar(electricity_price_MWh), 
         generation_MWh = number(generation_MWh, big.mark=",")) |>
  rename(`Pounds of CO2 Emitted per MWh of Electricity Produced`=CO2_MWh, 
         `Primary Source of Electricity Generation`=primary_source, 
         `Average Retail Price for 1000 kWh`=electricity_price_MWh, 
         `Total Generation Capacity (MWh)`= generation_MWh, 
         State=state)

#Question 1:
#Which state has the most expensive retail electricity?
EIA_SEP_REPORT_TOP_STATE <- EIA_SEP_REPORT |> 
  select(State,`Average Retail Price for 1000 kWh` ) |>
  mutate(`Average Retail Price for 1000 kWh` = as.numeric(str_replace_all(`Average Retail Price for 1000 kWh`, "[^0-9\\.]", ""))) |>
  arrange(desc(`Average Retail Price for 1000 kWh`))|> 
  slice(1)

EIA_SEP_REPORT_TOP_STATE <- EIA_SEP_REPORT_TOP_STATE |> 
  mutate(`Average Retail Price for 1000 kWh` = dollar(`Average Retail Price for 1000 kWh`))

#Question 2:
#Which state has the 'dirtiest' electricity mix?
# Step 1: Process and clean the data first, and assign it to the EIA_SEP_REPORT_DIRTIEST variable
EIA_SEP_REPORT_DIRTIEST <- EIA_SEP_REPORT |> 
  filter(!is.na(`Pounds of CO2 Emitted per MWh of Electricity Produced`)) |>  # Remove NAs
  select(State, `Pounds of CO2 Emitted per MWh of Electricity Produced`) |>
  # Clean the column, remove any non-numeric characters, and convert to numeric
  mutate(`Pounds of CO2 Emitted per MWh of Electricity Produced` = 
           as.numeric(str_replace_all(`Pounds of CO2 Emitted per MWh of Electricity Produced`, "[^0-9\\.]", ""))) |>
  arrange(desc(`Pounds of CO2 Emitted per MWh of Electricity Produced`)) |>  # Sort in descending order
  slice(1) |>  # Select the state with the highest emissions
  # Apply formatting after ensuring numeric type
  mutate(`Pounds of CO2 Emitted per MWh of Electricity Produced` = 
           number(`Pounds of CO2 Emitted per MWh of Electricity Produced`, big.mark=","))

datatable(EIA_SEP_REPORT, caption = htmltools::tags$caption(
  style = "font-size: 18px;", "Table 1: State Electricity Profiles"),
  options = list( autoWidth = TRUE),
  rownames = FALSE) |>
  formatStyle(columns = "Pounds of CO2 Emitted per MWh of Electricity Produced", 
              backgroundColor = "#f2f2f2", 
              fontWeight = 'bold') |>
  formatStyle(columns = "Average Retail Price for 1000 kWh", 
              backgroundColor = "#f2f2f2", 
              fontWeight = 'bold') |>
  formatStyle(columns = "Total Generation Capacity (MWh)", 
              backgroundColor = "#f2f2f2", 
              fontWeight = 'bold') |>
  formatStyle(columns = "Primary Source of Electricity Generation", 
              backgroundColor = "#f2f2f2", 
              fontWeight = 'bold') |>
  formatStyle(columns = "State",
              backgroundColor = "#f2f2f2", 
              fontWeight = 'bold')




# Step 2: Display the table with formatting and caption
# Display the table with formatting and caption
datatable(EIA_SEP_REPORT_DIRTIEST, 
          caption = htmltools::tags$caption(
            style = "font-size: 16px;", "Table 3: State with the dirtiest electricity mix"), rownames = FALSE,
          options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE)) |>
  formatStyle(columns = "Pounds of CO2 Emitted per MWh of Electricity Produced", 
              backgroundColor = "#FFCCCB", 
              fontWeight = 'bold')
#              
#Question 3: 
#On average, how many pounds of CO2 are emitted per MWh of electricity produced in the US? (Note that you will need to use a suitably weighted average here.)
EIA_SEP_REPORT_WEIGHTED_AVG <- EIA_SEP_REPORT |> 
  filter(!is.na(`Pounds of CO2 Emitted per MWh of Electricity Produced`)) |>  # Remove NAs
  select(`Pounds of CO2 Emitted per MWh of Electricity Produced`, `Total Generation Capacity (MWh)`) |>
  mutate(`Pounds of CO2 Emitted per MWh of Electricity Produced` = 
           as.numeric(str_replace_all(`Pounds of CO2 Emitted per MWh of Electricity Produced`, "[^0-9\\.]", ""))) |>
  mutate(`Total Generation Capacity (MWh)` =
           as.numeric(str_replace_all(`Total Generation Capacity (MWh)`, "[^0-9\\.]", ""))) |>
  # Calculate the weighted CO2 emissions
mutate(weighted_CO2 = `Pounds of CO2 Emitted per MWh of Electricity Produced` * `Total Generation Capacity (MWh)`) |>
summarise(weighted_avg_CO2 = sum(weighted_CO2) / sum(`Total Generation Capacity (MWh)`)) |>
  mutate(weighted_avg_CO2 = number(weighted_avg_CO2, big.mark=","))
#Display the result the weighted average CO2 emissions per MWh of electricity produced in the US
datatable(EIA_SEP_REPORT_WEIGHTED_AVG, 
          caption = htmltools::tags$caption(
            style = "font-size: 18px;", "Weighted Average of CO2 Emissions per MWh of Electricity Produced accross all states in the US") ,rownames = FALSE,
          options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE)) |>
  formatStyle(columns = "weighted_avg_CO2", 
              backgroundColor = "#FFCCCB", 
              fontWeight = 'bold')
#The weighted average is calculated using the total electricity generation capacity of each state.

#Question 4
#What is the rarest primary energy source in the US? What is the associated cost of electricity and where is it used?
EIA_SEP_REPORT_RAREST_ENERGY_SOURCE_STATE <- EIA_SEP_REPORT |>
  select(State, `Primary Source of Electricity Generation`, `Average Retail Price for 1000 kWh`) |>
  group_by(`Primary Source of Electricity Generation`) |>
  summarise(
    count = n(),
    `Average Retail Price for 1000 kWh` = mean(as.numeric(str_replace_all(`Average Retail Price for 1000 kWh`, "[^0-9\\.]", "")), na.rm = TRUE),
    State = names(sort(table(State), decreasing = TRUE))[1]  # Get the state with the most occurrences
  ) |>
  arrange(count) |>
  slice(1)  # This will give you the rarest energy source with the average price and most common state
EIA_SEP_REPORT_RAREST_ENERGY_SOURCE_STATE <- EIA_SEP_REPORT_RAREST_ENERGY_SOURCE_STATE |>
  mutate(`Average Retail Price for 1000 kWh` = dollar(`Average Retail Price for 1000 kWh`)) |>
  select(-count)  # Remove the count column
# Display result as a datatable
datatable(EIA_SEP_REPORT_RAREST_ENERGY_SOURCE_STATE, 
          caption = htmltools::tags$caption(
            style = "font-size: 18px;", 
            "Table 4: Rarest Primary Energy Source with Average Retail Price and Most Common State"
          ), rownames = FALSE,
          options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE)) |>
  formatStyle(columns = c("Average Retail Price for 1000 kWh"), 
              backgroundColor = "#f2f2f2", 
              fontWeight = 'bold')

#Question 5:
#How many times cleaner is NY’s energy mix than that of Texas?
EIA_SEP_REPORT_CLEANER_MIX <- EIA_SEP_REPORT |>
  select(State, `Pounds of CO2 Emitted per MWh of Electricity Produced`) |>
  filter(State %in% c("New York", "Texas")) |>
  mutate(`Pounds of CO2 Emitted per MWh of Electricity Produced` = as.numeric(str_replace_all(`Pounds of CO2 Emitted per MWh of Electricity Produced`, "[^0-9\\.]", ""))) |>
  arrange(State) |>  # Arrange the data by State for clarity
  group_by(State) |>  # Group by state to compute values for each one separately
  summarise(
    `Pounds of CO2 Emitted per MWh of Electricity Produced` = mean(`Pounds of CO2 Emitted per MWh of Electricity Produced`, na.rm = TRUE),
    .groups = 'drop'
  ) |>  # Ensure no lingering grouping after summarising
  mutate(ratio = `Pounds of CO2 Emitted per MWh of Electricity Produced`[State == "New York"] /
           `Pounds of CO2 Emitted per MWh of Electricity Produced`[State == "Texas"]) |>
  mutate(ratio = round(ratio, 2))  


datatable(EIA_SEP_REPORT_CLEANER_MIX, rownames = FALSE, caption = htmltools::tags$caption(
  style = "font-size: 18px;","Table 5: New York's energy mix compared to Texas's energy mix (by cleanliest)"),
  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE)) |>
  formatStyle(columns = "State",  # Format based on the "State" column
              target = 'row',  # Apply style to entire row
              backgroundColor = styleEqual(c("New York", "Texas"), c("#f2f2f2", "#DCDCDC")),  # Different colors for New York and Texas
              fontWeight = 'bold')

#Data Import Part 2
#Annual Database Energy Consumption 2023  
ensure_package(readxl)
# Create 'data/mp02' directory if not already present
DATA_DIR <- file.path("data", "mp02")
dir.create(DATA_DIR, showWarnings=FALSE, recursive=TRUE)

NTD_ENERGY_FILE <- file.path(DATA_DIR, "2023_ntd_energy.xlsx")

if(!file.exists(NTD_ENERGY_FILE)){
  DS <- download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-10/2023%20Energy%20Consumption.xlsx", 
                      destfile=NTD_ENERGY_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_ENERGY_FILE)$size == 0)){
    cat("I was unable to download the NTD Energy File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_ENERGY_RAW <- read_xlsx(NTD_ENERGY_FILE)

ensure_package(tidyr)
to_numeric_fill_0 <- function(x){
  x <- if_else(x == "-", NA, x)
  replace_na(as.numeric(x), 0)
}
#Clean up data
NTD_ENERGY <- NTD_ENERGY_RAW |> 
  select(-c(`Reporter Type`, 
            `Reporting Module`, 
            `Other Fuel`, 
            `Other Fuel Description`)) |>
  mutate(across(-c(`Agency Name`, 
                   `Mode`,
                   `TOS`), 
                to_numeric_fill_0)) |>
  group_by(`NTD ID`, `Mode`, `Agency Name`) |>
  summarize(across(where(is.numeric), sum), 
            .groups = "keep") |>
  mutate(ENERGY = sum(c_across(c(where(is.numeric))))) |>
  filter(ENERGY > 0) |>
  select(-ENERGY) |>
  ungroup()

# Display 10 random rows
slice_sample(NTD_ENERGY , n=10)
unique(NTD_ENERGY$Mode)

# Get unique Mode codes from the data
unique_modes <- NTD_ENERGY |>
  distinct(Mode)  # This will return only the unique Mode values

# Display the unique Mode codes
print(unique_modes)
datatable(unique_modes, caption = "Unique Modes in NTD_Energy")


#Recoding the Mode Column - changing the names of the unique modes in the Mode column 
NTD_ENERGY <- NTD_ENERGY |>
  mutate(Mode=case_when(
    Mode == "DR"  ~ "Demand Response",  
    Mode == "FB"  ~ "Ferryboat",
    Mode == "MB"  ~ "Motorbus" ,
    Mode == "SR"  ~ "Street Car Rail",
    Mode == "TB"  ~ "Trolleybus",
    Mode == "VP"  ~ "Vanpool",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "RB" ~ "Rapid Bus" , 
    Mode == "LR" ~ "Light Rail",
    Mode == "MG" ~ "Monorail Guideway",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "AR" ~ "Alaska Railroad",  
    Mode == "TR" ~ "Aerial Tramway",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "YR" ~ "Hybrid Rail",  
    Mode == "IP" ~ "Inclined Plane Vehicle", 
    Mode == "PB" ~ "Publico Mode",
    Mode == "CC" ~ "Cable Car",
    TRUE ~ "Unknown"))
#Data Import Part 3
# Annual Database Service by Agency 2023
# 
ensure_package(readr)
NTD_SERVICE_FILE <- file.path(DATA_DIR, "2023_service.csv")
if(!file.exists(NTD_SERVICE_FILE)){
  DS <- download.file("https://data.transportation.gov/resource/6y83-7vuw.csv", 
                      destfile=NTD_SERVICE_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_SERVICE_FILE)$size == 0)){
    cat("I was unable to download the NTD Service File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_SERVICE_RAW <- read_csv(NTD_SERVICE_FILE)

#Clean up data
NTD_SERVICE <- NTD_SERVICE_RAW |>
  mutate(`NTD ID` = as.numeric(`_5_digit_ntd_id`)) |> 
  rename(Agency = agency, 
         City   = max_city, 
         State  = max_state,
         UPT    = sum_unlinked_passenger_trips_upt, 
         MILES  = sum_passenger_miles) |>
  select(matches("^[A-Z]", ignore.case=FALSE)) |>
  filter(MILES > 0)


#Task 4: Explore NTD Service Data
#Which transit service has the most UPT annually?
NTD_SERVICE_HighUPT <- NTD_SERVICE |>
  # Find the transit service with the most UPT annually
  group_by(Agency, City, State) |>
  summarize(Total_UPT = sum(UPT, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(Total_UPT)) |>
  slice(1)|>  # Get the transit service with the most UPT
  mutate(Total_UPT = scales::comma(Total_UPT))  # Add commas to Total UPT
# Display the result
datatable(NTD_SERVICE_HighUPT, caption = "Transit Service with the most YPT annually" , rownames = FALSE) |>
  formatStyle("Total_UPT",
              backgroundColor = "lightgray", 
              fontWeight = 'bold') 


# Filter the dataset for the cities of interest (Brooklyn, New York, Staten Island)
# and for the specific agency "MTA New York City Transit"
mta_nyc_trip_data <- NTD_SERVICE |>
  filter(Agency == "MTA New York City Transit")  # Filtering for MTA New York City Transit in the Agency column

# Check the unique values in the Agency column to ensure it's correctly filtering
unique(mta_nyc_trip_data$Agency)

# Calculate the average trip length 'MILES' is the trip length column
average_trip_length <- mta_nyc_trip_data |>
  summarize(average_trip_length = mean(MILES, na.rm = TRUE))

# Print the average trip length
print(average_trip_length)

# Create a formatted datatable for the result
datatable(average_trip_length, caption = "The average trip length of a trip on MTA New York City Transit in miles", 
          rownames = FALSE, 
          options = list(pageLength = 5, dom = 't')) |>
  formatRound(columns = "average_trip_length", digits = 0) |>
  formatStyle("average_trip_length", 
              backgroundColor = styleInterval(0, c("lightgreen", "lightblue")),
              fontWeight = 'bold', 
              color = "black")

#Which transit service in NYC has the longest average trip length?

longest_avg_trip_length <- NTD_SERVICE |>
  filter(City %in% c("Brooklyn", "New York", "Staten Island")) |>
  group_by(Agency) |>
  summarize(average_trip_length = mean(MILES, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(average_trip_length))  # Sort by the longest average trip length

# Print the transit service with the longest average trip length
print(longest_avg_trip_length)

# Create a formatted datatable for the result
datatable(longest_avg_trip_length, caption = "Transit Services with Longest Average Trip Length in NYC", 
          rownames = FALSE, 
          options = list(pageLength = 5, dom = 't')) |>
  formatRound(columns = "average_trip_length", digits = 0) |>
  formatStyle("average_trip_length", 
              backgroundColor = styleInterval(0, c("lightgreen", "lightblue")),
              fontWeight = 'bold', 
              color = "black")

#Which state has the fewest total miles traveled by public transit?
state_total_miles <- NTD_SERVICE |>
  group_by(State) |>
  summarize(total_miles = sum(MILES, na.rm = TRUE), .groups = "drop") |>
  arrange(total_miles)  # Sort in ascending order to find the state with the fewest miles

# Get the state with the fewest miles
state_with_fewest_miles <- state_total_miles[1, ]  # The first row will be the state with the fewest miles

# Print the state with the fewest miles
print(state_with_fewest_miles)
datatable(state_with_fewest_miles, 
          caption = "State with the Fewest Total Miles Traveled by Public Transit",
          rownames = FALSE, 
          options = list(pageLength = 5, dom = 't')) |>
  formatRound(columns = "total_miles", digits = 0) |>
  formatStyle("total_miles", 
              backgroundColor = styleInterval(0, c("lightblue", "lightgreen")),
              fontWeight = 'bold', 
              color = "black")

#Are all states represented in this data? If no, which ones are missing?
# Get the list of all U.S. state abbreviations
all_state_abb <- state.abb

# Get the list of unique state abbreviations present in your dataset
states_in_data <- unique(NTD_SERVICE$State)

# Find missing state abbreviations by comparing with all_state_abb
missing_abb_states <- setdiff(all_state_abb, states_in_data)

# Combine the missing state abbreviations and their full names into one data frame
missing_combined <- data.frame(
  Missing_State_Abbreviation = missing_abb_states,
  Missing_State_Full_Name = state.name[match(missing_abb_states, state.abb)]  # Mapping abbreviations to full names
)

# Display the result in a formatted datatable
datatable(missing_combined, 
          caption = "Missing States in the Data (Abbreviation and Full Name)",
          rownames = FALSE, 
          options = list(pageLength = nrow(missing_combined), dom = 't')) |>
  formatStyle(c("Missing_State_Full_Name", "Missing_State_Abbreviation"), 
              backgroundColor = styleInterval(0, c("lightblue", "lightcoral")),
              fontWeight = 'bold', 
              color = "black")

#Task 5: Calculate Emissions

#Joining the three datasets
# Step 1: Left join NTD_SERVICE and NTD_ENERGY by NTD ID
merged_data <- left_join(NTD_SERVICE, NTD_ENERGY, by = "NTD ID")

# Step 2: Left join with EIA_SEP_REPORT by NTD_SERVICE's "State" column
merged_data <- left_join(merged_data, EIA_SEP_REPORT, by = "State")

# Step 4: Ensure the required columns are included
merged_data <- merged_data |>
  select(Agency, Mode, `NTD ID`, City, State, UPT, MILES, 
         `Bio-Diesel`, `Bunker Fuel`, `C Natural Gas`, `Diesel Fuel`, 
         `Electric Battery`, `Electric Propulsion`, `Ethanol`, 
         `Methonal`, `Gasoline`, `Hydrogen`, `Kerosene`, 
         `Liquified Nat Gas`, `Liquified Petroleum Gas`, 
         `Pounds of CO2 Emitted per MWh of Electricity Produced`)

# Step 5: View the data
View(merged_data)

#Calculate Total Emissions
total_emissions_data <- merged_data |>
  mutate(
    total_emissions = (
      `Bio-Diesel` *22.45 + 
        `Bunker Fuel` * 24.78 + 
        `C Natural Gas` * 18.32 + 
        `Diesel Fuel` * 22.45 + 
        `Electric Battery` * 0 + 
        `Electric Propulsion` * 0 + 
        `Ethanol` * 19.94 + 
        `Methonal` * 19.94  + 
        `Gasoline` * 20.86 +
        `Hydrogen` *  0 + 
        `Kerosene` * 21.78 + 
        `Liquified Nat Gas` * 120.85 + 
        `Liquified Petroleum Gas` * 33.04
    )
  )

View(total_emissions_data)

# Task 6: Normalize Emissions to Transit Usage
# Compute the emission per UPT
emission_per_UPT <- total_emissions_data |>
  mutate(per_UPT = total_emissions / UPT)

# Now, calculate the categorization of per_UPT
sum_UPT <- emission_per_UPT |>
  mutate(
    calc_cat_UPT = case_when(
      is.na(per_UPT) ~ "Unknown",  # Handle NA values first
      per_UPT < 2 ~ "Small",       # Values less than 2
      per_UPT >= 2 & per_UPT <= 4 ~ "Medium",  # Values between 2 and 4
      per_UPT > 4 ~ "Large",       # Values greater than 4
      TRUE ~ "Unknown"             # Fallback if none of the conditions are met
    )
  )

# View the result
#View(sum_UPT)
#Display results in a datatable
## Create the datatable
datatable(sum_UPT, 
          caption = "Categorization of Emissions per UPT", 
          rownames = FALSE, 
          options = list(pageLength = 10)) |>
  formatStyle("calc_cat_UPT", 
              backgroundColor = styleEqual(c("Small", "Medium", "Large", "Unknown"), 
                                           c("lightgreen", "lightyellow", "lightblue", "lightgray")),
              fontWeight = 'bold', 
              color = "black")

# Task: Compute the emission per passenger mile
emission_per_passmile <- total_emissions_data |>
  mutate(per_pass_mile = total_emissions / MILES)

# Now, calculate the emission per passenger mile
sum_passmile <- emission_per_passmile |>
  mutate(
    calc_cat_passmile = case_when(
      is.na(per_pass_mile) ~ "Unknown",  # Handle NA values first
      per_pass_mile < 0.1 ~ "Small",     # Emissions less than 0.1
      per_pass_mile >= 0.1 & per_pass_mile <= 0.3 ~ "Medium",  # Emissions between 0.1 and 0.3
      per_pass_mile > 0.3 ~ "Large",     # Emissions greater than 0.3
      TRUE ~ "Unknown"                   # Fallback if none of the conditions are met
    )
  )

# View the result
View(sum_passmile)


# Create the datatable for the 'sum_passmile' dataset
datatable(sum_passmile, 
          caption = "Emissions per Passenger Mile by Agency", 
          rownames = FALSE, 
          options = list(pageLength = 10, dom = 't')) |>
  formatStyle("calc_cat_passmile", 
              backgroundColor = styleEqual(c("Small", "Medium", "Large", "Unknown"), 
                                           c("lightgreen", "lightyellow", "lightblue", "lightgray")),
              fontWeight = 'bold', 
              color = "black") |>
  formatRound(columns = c("per_pass_mile"),
              digits = 3)  # Format the emission per passenger mile column to 3 decimal places

# Compute Total Emissions, Emissions per UPT, and Emissions per Passenger Mile at the Agency Level
agency_emissions <- total_emissions_data |>
  group_by(Agency) |>
  summarize(
    # Sum total emissions across modes for each Agency
    total_agency_emissions = sum(total_emissions, na.rm = TRUE),
    
    # Sum UPT and MILES for each Agency (needed for normalization)
    total_UPT = sum(UPT, na.rm = TRUE),
    total_miles = sum(MILES, na.rm = TRUE),
    
    # Calculate Emission per UPT for each Agency
    emission_per_UPT = total_agency_emissions / total_UPT,
    
    # Calculate Emission per Passenger Mile for each Agency
    emission_per_passmile = total_agency_emissions / total_miles
  ) |>
  mutate(
    # Categorize Emission per UPT (optional)
    calc_cat_UPT = case_when(
      is.na(emission_per_UPT) ~ "Unknown",
      emission_per_UPT < 2 ~ "Small",
      emission_per_UPT >= 2 & emission_per_UPT <= 4 ~ "Medium",
      emission_per_UPT > 4 ~ "Large",
      TRUE ~ "Unknown"
    ),
    
    # Categorize Emission per Passenger Mile (optional)
    calc_cat_passmile = case_when(
      is.na(emission_per_passmile) ~ "Unknown",
      emission_per_passmile < 0.1 ~ "Small",
      emission_per_passmile >= 0.1 & emission_per_passmile <= 0.3 ~ "Medium",
      emission_per_passmile > 0.3 ~ "Large",
      TRUE ~ "Unknown"
    )
  )

# View the resulting summarized and categorized emissions data for each Agency
View(agency_emissions)

# Create the datatable for the 'agency_emissions' dataset
datatable(agency_emissions, 
          caption = "Agency-Level Emissions Data", 
          rownames = FALSE, 
          options = list(pageLength = 10, dom = 't')) |>
  formatStyle("calc_cat_UPT", 
              backgroundColor = styleEqual(c("Small", "Medium", "Large", "Unknown"), 
                                           c("lightgreen", "lightyellow", "lightblue", "lightgray")),
              fontWeight = 'bold', 
              color = "black") |>
  formatStyle("calc_cat_passmile", 
              backgroundColor = styleEqual(c("Small", "Medium", "Large", "Unknown"), 
                                           c("lightgreen", "lightyellow", "lightblue", "lightgray")),
              fontWeight = 'bold', 
              color = "black") |>
  formatRound(columns = c("total_agency_emissions", "emission_per_UPT", "emission_per_passmile"),
              digits = 2)  # Format the emissions columns to 2 decimal places


#Greenest Transit Agencies
## Determine the Greenest Transit Agency (the agency with the lowest emissions per UPT)
greenest_agency <- agency_emissions |>
  arrange(emission_per_UPT) |>
  slice(1) # Agency with the lowest emissions per UPT

# Display the result
View(greenest_agency)

# Display as an interactive table
datatable(greenest_agency, 
          caption = "Greenest Transit Agency or Lowest Emissions per UPT", 
          rownames = FALSE, 
          options = list(pageLength = 5)) |>
  formatStyle("emission_per_UPT", 
              backgroundColor = styleInterval(0, c("lightgreen", "lightblue")),
              fontWeight = 'bold', 
              color = "black")


# Compute the percentage of electrification for each agency
agency_electrification <- total_emissions_data |>
  mutate(
    # Calculate total energy consumed
    total_energy_consumed = rowSums(across(c(`Bio-Diesel`, `Bunker Fuel`, `C Natural Gas`, 
                                           `Diesel Fuel`, `Electric Battery`, `Electric Propulsion`, 
                                           `Ethanol`, `Methonal`, `Gasoline`, `Hydrogen`, 
                                           `Kerosene`, `Liquified Nat Gas`, `Liquified Petroleum Gas`)), 
                                    na.rm = TRUE),
    
    # Calculate percentage of energy from electric sources
    electrification_percentage = (`Electric Battery` + `Electric Propulsion`) / total_energy_consumed * 100
  ) |>
  group_by(Agency) |>
  summarize(
    electrification_percentage = mean(electrification_percentage, na.rm = TRUE)
  ) |>
  arrange(desc(electrification_percentage))

# Agency with the highest percentage of electrification
agency_with_highest_electrification <- agency_electrification |>
  arrange(desc(electrification_percentage)) |>
  slice(1)

# Display as an interactive table
datatable(agency_with_highest_electrification, 
          caption = "Highest Percentage of Electrification", 
          rownames = FALSE, 
          options = list(pageLength = 5)) |>
  formatStyle("electrification_percentage", 
              backgroundColor = styleInterval(0, c("lightgreen", "lightblue")),
              fontWeight = 'bold', 
              color = "black")

# Display the result
View(agency_with_highest_electrification)

# Determine the agency with the highest total emissions
worst_agency <- agency_emissions |>
  arrange(desc(total_agency_emissions)) |>
  slice(1)

# Display the result
View(worst_agency)

# Worst Agency (highest total emissions)
worst_agency <- agency_emissions |>
  arrange(desc(total_agency_emissions)) |>
  slice(1)

# Display as an interactive table
datatable(worst_agency, 
          caption = "Worst Agency (Highest Total Emissions)", 
          rownames = FALSE, 
          options = list(pageLength = 5)) |>
  formatStyle("total_agency_emissions", 
              backgroundColor = styleInterval(0, c("lightcoral", "lightyellow")),
              fontWeight = 'bold', 
              color = "black")


#Categorize Smaller agencies: Best Small Agency
## Categorizing agencies based on total UPT size
agency_size_category <- agency_emissions |>
  mutate(
    size_category = case_when(
      total_UPT < 500000 ~ "Small",
      total_UPT >= 500000 & total_UPT < 20000000 ~ "Medium",
      total_UPT >= 20000000 ~ "Large",
      TRUE ~ "Unknown"
    )
  )

# Display categorized agencies in an interactive table
datatable(agency_size_category, 
          caption = "Transit Agencies Categorized by Size (Total UPT)", 
          rownames = FALSE, 
          options = list(pageLength = 10)) |>
  formatStyle("size_category", 
              backgroundColor = styleEqual(c("Small", "Medium", "Large"), 
                                           c("lightgreen", "lightyellow", "lightblue")),
              fontWeight = 'bold', 
              color = "black")

#Data Visualization 
library(ggplot2)

# Create a bar chart to visualize the distribution of agencies by size category
ggplot(agency_size_category, aes(x = size_category, fill = size_category)) +
  geom_bar() + # Create bars for each size category
  labs(
    title = "Distribution of Transit Agencies by Size (Total UPT)",
    x = "Size Category",
    y = "Number of Agencies"
  ) +
  scale_fill_manual(values = c("lightgreen", "lightyellow", "lightblue")) + # Colors for each category
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for readability
  )

#Display Awards by Category
## Display Best Small Agency (the winner within the "Small" category)
best_small_agency <- agency_size_category |>
  filter(size_category == "Small") |>
  arrange(emission_per_UPT) |>
  slice(1)

# Display as an interactive table
datatable(best_small_agency, 
          caption = "Best Small Agency (Lowest Emissions per UPT in Small Agencies)", 
          rownames = FALSE, 
          options = list(pageLength = 5)) |>
  formatStyle("emission_per_UPT", 
              backgroundColor = styleInterval(0, c("lightgreen", "lightblue")),
              fontWeight = 'bold', 
              color = "black")

## Display Best Medium Agency (the winner within the "Medium" category)
best_med_agency <- agency_size_category |>
  filter(size_category == "Medium") |>
  arrange(emission_per_UPT) |>
  slice(1)


# Display as an interactive table
datatable(best_med_agency, 
          caption = "Best Medium Agency (Lowest Emissions per UPT in Medium Agencies)", 
          rownames = FALSE, 
          options = list(pageLength = 5)) |>
  formatStyle("emission_per_UPT", 
              backgroundColor = styleInterval(0, c("purple", "navy")),
              fontWeight = 'bold', 
              color = "white")


## Display Best Large Agency (the winner within the "Large" category)
best_large_agency <- agency_size_category |>
  filter(size_category == "Large") |>
  arrange(emission_per_UPT) |>
  slice(1)

# Display as an interactive table
datatable(best_large_agency, 
          caption = "Best Large Agency (Lowest Emissions per UPT in Large Agencies)", 
          rownames = FALSE, 
          options = list(pageLength = 5)) |>
  formatStyle("emission_per_UPT", 
              backgroundColor = styleInterval(0, c("orange", "red")),
              fontWeight = 'bold', 
              color = "white")







