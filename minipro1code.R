if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}
library(readr)
payroll_data <- read_csv("data/mp01/nyc_payroll_export.csv")
View(payroll_data)
library(dplyr)
library(stringr)
payroll_data <- payroll_data |> 
  mutate(
    agency_name = str_to_title(agency_name),
    last_name  = str_to_title(last_name),
    first_name = str_to_title(first_name),
    work_location_borough = str_to_title(work_location_borough),
    title_description = str_to_title(title_description),
    leave_status_as_of_june_30 = str_to_title(leave_status_as_of_june_30)
     ) |>
  rename(
    `Fiscal Year`    = fiscal_year,
    `Agency Name`    = agency_name,
    `First Name`     = first_name,
    `Middle Initial` = mid_init,
    `Last Name`      = last_name,
    `Position`       = title_description,
    `Base Salary`    = base_salary,
    `Start Date`     = agency_start_date,
    `Reg Hours`      = regular_hours,
    `Overtime Hours` = ot_hours,
    `Additional Pay` = total_other_pay,
    `Payroll Number` = payroll_number,
    `Work Location`  = work_location_borough,
    `Leave Status`   = leave_status_as_of_june_30,
    `Pay Basis`      = pay_basis,
    `Gross Paid`     = regular_gross_paid,
    `Overtime Paid`  = total_ot_paid
  ) 
View(payroll_data)
#filter out negative regular_gross_paid from payroll_data
payroll_data_filtered <- payroll_data |>
  filter(`Gross Paid` >= 0) 
View(payroll_data_filtered)

eric_adams_data <- payroll_data_filtered |>
    filter(`First Name` == "Eric", `Middle Initial` == "L", `Last Name` == "Adams") |>
    mutate(`Total Salary` = `Gross Paid` + `Overtime Paid` + `Additional Pay`) |>
  group_by(`Fiscal Year`) 

eric_adams_career <- eric_adams_data |>
  group_by(`Fiscal Year`, `Agency Name`) |>
  summarise(
    `Total Salary` = sum(`Total Salary`, na.rm = TRUE),
    `Position` = paste(unique(`Position`), collapse = ","),
    .groups = "drop"
    ) 
  #arrange(`Fiscal Year`)
View(eric_adams_career)
    #total_gross_pay = regular_gross_paid + total_ot_paid + total_other_pay


library(DT)
datatable(eric_adams_career, caption = 'Table 1: This table displays the career of NYC Mayor Eric L Adams.', options = list(pageLength = nrow(eric_adams_career), autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Salary"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Total Salary"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Position"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Total Salary"), fontWeight = "bold", color = "#333333")

#compute the actual total compensation for each employee record in the payroll_data_filtered 
#using the case_when() and  mutate() command to base salary greater than 100 and base salary less than 100
#add a new column called 'Total Compensation' to the payroll_data
# no use of overtime paid, additional pay, or gross paid
#
payroll_data_comp <- payroll_data_filtered |>
  mutate(
    `Total Compensation` = case_when(
      `Position` == "Mayor" ~ `Base Salary`,
      !is.na(`Base Salary`) & `Base Salary` < 100  ~(`Base Salary`* (`Reg Hours` + `Overtime Hours` * 1.5)),
      !is.na(`Base Salary`) & `Base Salary` >= 100 ~ ((`Base Salary`/260) * (`Reg Hours` / 7.5)),
       #pay_basis == "Annual" ~ `Base Salary` + `Overtime Paid` + `Additional Pay`,
       #pay_basis == "Hourly" ~ `Base Salary` * 40 * 52,
       #pay_basis == "Daily" ~ `Base Salary` * 260,
       TRUE ~ NA_real_
     )
   ) 
View(payroll_data_comp)
datatable(head(payroll_data_comp), caption = 'Table 2: This table displays the total compensation for each employee in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Compensation"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Total Compensation"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Position"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Total Compensation"), fontWeight = "bold", color = "#333333")

#Which 'Position' has the highest 'Base Salary'? (If needed, assume a standard 2000 hour work year and no overtime.
highest_base_salary <- payroll_data_comp |>
  group_by(`Position`) |>
  summarise(
  `Total Compensation` = mean(`Base Salary`, na.rm = TRUE)
  ) |>
  arrange(desc(`Total Compensation`)) |>
  head(1)
datatable(highest_base_salary, caption = 'Table 2: This table displays the position with the highest base salary in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Compensation"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Total Compensation"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Position"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Total Compensation"), fontWeight = "bold", color = "#333333")
View(highest_base_salary)

#  Which individual  & in what year had the single highest city total payroll( regular and overtime) in the dataset?
highest_payroll <- payroll_data_filtered |>
  mutate(`Total Payroll` = `Base Salary` + `Overtime Paid`) |>
  slice_max(order_by = `Total Payroll`, n = 1, with_ties = FALSE)
View(highest_payroll)

datatable(highest_payroll, caption = 'Table 3: This table displays the individual with the highest total payroll in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Payroll"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Total Payroll"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Position"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Total Payroll"), fontWeight = "bold", color = "#333333")


#Which person worked the most overtime hours in payroll_data_filtered?
most_overtime_hours <- payroll_data_filtered |>
group_by(`Fiscal Year`,`Agency Name` , `Position`, `First Name`, `Last Name`) |>
  summarise(
  `Total Overtime` = max(`Overtime Hours`, na.rm = TRUE), .groups = "drop"
  ) |>
  slice_max(order_by = `Total Overtime`, n = 6, with_ties = TRUE)
datatable(most_overtime_hours, caption = 'Table 4: This table displays the individuals with the most overtime hours in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatStyle(c("Total Overtime"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("First Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Last Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Position"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Total Overtime"), fontWeight = "bold", color = "#333333")
#View(most_overtime_hours)
#View(payroll_data_filtered |> mutate(`Total Payroll` = `Base Salary` + `Overtime Paid`))
#summarise(
#`Total Payroll` = mean(`Total Payroll`, na.rm = TRUE), .groups = "drop"
#) |>
# Find individual with the highest regular hours
#highest_reg_hours <- payroll_data_filtered |>
  #arrange(desc(`Reg Hours`)) |>
  #head(1)  # Get the row with the highest regular hours

# View the result
#View(highest_reg_hours)

# Find the individual with the highest overtime hours
#highest_overtime_hours <- payroll_data_filtered |>
  #arrange(desc(`Overtime Hours`)) |>
  #head(1)  # Get the row with the highest overtime hours

# View the result
#View(highest_overtime_hours)

#Find individual with the highest total hours
#highest_total_hours <- payroll_data_filtered |>
  #mutate(
 # `Total Hours` = `Reg Hours` + `Overtime Hours`
 # ) |>
 # arrange(desc(`Total Hours`)) |>
  #head(1)  # Get the row with the highest total hours
  #View(highest_total_hours)

#Find Which agency has the highest average total annual payroll (base salary and overtime pay per employee)?
avg_total_payroll <- payroll_data_comp |>
  mutate(`Total Annual Payroll` = `Base Salary` + `Overtime Paid`) |>
  group_by(`Fiscal Year`, `Agency Name`) |>
  summarise(
    `Total Annual Payroll` = mean(`Total Annual Payroll`, na.rm = TRUE), .groups = "drop"
  ) |>
  arrange(desc(`Total Annual Payroll`)) |>
  slice_max(order_by = `Total Annual Payroll`, n = 10, with_ties = FALSE)
#View(avg_total_payroll)
#
datatable(avg_total_payroll, caption = 'Table 5: This table displays the agency with the highest average total annual payroll in the NYC payroll data.', options = list(pageLength = 10, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Annual Payroll"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Total Annual Payroll"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Total Annual Payroll"), fontWeight = "bold", color = "#333333")

# Find Which agency has the most employees on payroll in each year?
agency_employee_count <- payroll_data_comp |>
 group_by(`Fiscal Year`, `Agency Name`) |>
 summarise(
 `Employee Count` = n(), .groups = "drop"
 ) |>
 arrange(`Fiscal Year`, desc(`Employee Count`)) |> 
 group_by(`Fiscal Year`) |>
 slice_head(n = 1)  # Keeps only the agency with the largest employee count each year
#View(agency_employee_count)

datatable(agency_employee_count, caption = 'Table 6: This table displays the agency with the most employees on payroll in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatStyle(c("Employee Count"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Employee Count"), fontWeight = "bold", color = "#333333")

# Which agency has the highest overtime hours (compared to reg hours)?


# Find agency with the highest regular hours
highest_reg_hours <- payroll_data_filtered |>
  group_by(`Agency Name`) |>
  summarise(
    `Average Reg Hours` = mean(`Reg Hours`, na.rm = TRUE), .groups = "drop"
  ) |>
  slice_max(order_by = `Average Reg Hours`, n = 1, with_ties = FALSE)

#View(highest_reg_hours)

#Find agency with the highest overtime hours
highest_overtime_agency <- payroll_data_filtered |>
  group_by(`Agency Name`) |>
  summarise(
    `Average Overtime Hours` = mean(`Overtime Hours`, na.rm = TRUE), .groups = "drop"
  ) |>
  slice_max(order_by = `Average Overtime Hours`, n = 1, with_ties = FALSE)
#View(highest_overtime_agency)
combine_data <- highest_reg_hours |>
  left_join(highest_overtime_agency, by = "Agency Name") 

View(combine_data)

datatable(combine_data, caption = 'Table 7: This table displays the agency with the highest average regular and overtime hours in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatRound(c("Average Reg Hours", "Average Overtime Hours"), digits = 2) |>
  formatStyle(c("Average Reg Hours"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Average Overtime Hours"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Average Reg Hours"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Average Overtime Hours"), fontWeight = "bold", color = "#333333")


#What is the average `Base Salary` of employees who's `Work Location` != "Brooklyn", "Bronx", "Queen", "Manhattan", "Staten Island"?
salary_non_nyc <- payroll_data_comp |>
  filter(`Work Location` != "Brooklyn", `Work Location` != "Bronx", `Work Location` != "Queen", `Work Location` != "Manhattan", `Work Location` != "Staten Island") |>
  group_by(`Work Location`) |>
  summarise(
  `Average Base Salary` = mean(`Base Salary`, na.rm = TRUE), .groups = "drop"
  ) |>
  arrange(desc(`Average Base Salary`))
View(salary_non_nyc)

datatable(salary_non_nyc, caption = 'Table 8: This table displays the average base salary of employees with work locations outside of NYC in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Average Base Salary"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Average Base Salary"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Work Location"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Average Base Salary"), fontWeight = "bold", color = "#333333")

#New York City's aggregate payroll over the past 10 years
#How much has the `Total Payroll` changes from `Fiscal Year` == 2014 to `Fiscal Year==2024?
library(tidyr)
aggregate_growth <- payroll_data_comp |>
  filter(`Fiscal Year` %in% c("2014", "2024")) |>
  group_by(`Fiscal Year`) |>
  summarise(
    `Aggregate Payroll` = sum(`Base Salary`, na.rm = TRUE), .groups = "drop"
  ) |>
  pivot_wider(names_from = `Fiscal Year`, values_from = `Aggregate Payroll`)  |>
  mutate(
    `Growth Rate` = (`2024` - `2014`) / `2014`
 ) 
#View(aggregate_growth)
datatable(aggregate_growth, caption = 'Table 9: This table displays the aggregate payroll growth from 2014 to 2024 in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("2014", "2024"), currency = "$", interval = 3, mark = ",") |>
  formatPercentage(c("Growth Rate"), digits = 2) |>
  formatStyle(c("Growth Rate"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Growth Rate"), fontWeight = "bold", color = "#333333")

# Policy #1 Calculations: 

# Compute the Total Pay for 'Position' == "Mayor" for each 'Fiscal Year'
total_pay_mayor <- payroll_data_comp |>
  mutate(`Total Mayor Pay` = `Base Salary` + `Overtime Paid`) |>
  group_by(`Fiscal Year`) |>
  filter(`Position` == "Mayor") |>
  summarise(
    `Total Mayor Pay` = sum(`Total Mayor Pay`, na.rm = TRUE), .groups = "drop"
  ) |>
  arrange(`Fiscal Year`)
datatable(total_pay_mayor, caption = 'Table 9: This table displays the total pay for the Mayor in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Mayor Pay"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Total Mayor Pay"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Total Mayor Pay"), fontWeight = "bold", color = "#333333")
# Find employees who made more than the Mayor each 'Fiscal Year'
more_than_mayor <- payroll_data_comp |>
  mutate(`Total Pay` = `Base Salary` + `Overtime Paid`) |>
  filter(`Position` != "Mayor") |>
  left_join(total_pay_mayor, by = "Fiscal Year") |>
  filter(`Total Pay` >= `Total Mayor Pay`) |>
  group_by(`Fiscal Year`, `First Name`, `Middle Initial`, `Last Name`) |>
  summarise(
    `Total Pay` = sum(`Total Pay`, na.rm = TRUE), .groups = "drop"
  ) |>
  arrange(`Fiscal Year`, desc(`Total Pay`)) |>
  group_by(`Fiscal Year`) |> 
  slice_head(n = 10)  # Select top 10 employees with the highest total pay in each fiscal year

# View the results
View(more_than_mayor)

datatable(more_than_mayor, caption = 'Table 10: This table displays the employees who made more than the Mayor in the NYC payroll data.', options = list(pageLength = 5, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Pay"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Total Pay"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("First Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Middle Initial"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Last Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Total Pay"), fontWeight = "bold", color = "#333333")

# Find the `Total Savings` if the Mayor's salary is the maximum salary for each `Fiscal Year`
total_savings <- payroll_data_comp |>
  filter(`Position` != "Mayor") |>
  left_join(total_pay_mayor, by = "Fiscal Year") |>
  group_by(`Fiscal Year`) |>
  summarise(
  `Total Savings` = max(`Base Salary`, na.rm = TRUE) - unique(`Total Mayor Pay`), .groups = "drop"
  ) |>
  arrange(`Fiscal Year`)
View(total_savings)

datatable(total_savings, caption = 'Table 11: This table displays the total savings if the Mayor\'s salary was the maximum salary for each fiscal year in the NYC payroll data.', options = list(pageLength = 10, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Savings"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Total Savings"), backgroundColor = "#f2f2f2") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Total Savings"), fontWeight = "bold", color = "#333333")

# Find the `Agency Name` and `Position` of employees who made more than the Mayor each `Fiscal Year`
employees_more_than_mayor <- payroll_data_comp |>
  mutate(`Total Employee Pay` = `Base Salary` + `Overtime Paid`) |>
  filter(`Position` != "Mayor") |>
  left_join(total_pay_mayor, by = "Fiscal Year") |>
  filter(`Total Employee Pay` >= `Total Mayor Pay`) |>
  select(`Agency Name`, `Position`, `Fiscal Year`, `Total Employee Pay`, `Total Mayor Pay`) |>
  arrange(`Fiscal Year`, desc(`Total Employee Pay`))
  #group_by(`Fiscal Year`) |>
  #slice_head(n = 10)  # Select top 20 employees
View(employees_more_than_mayor)
datatable(employees_more_than_mayor, caption = 'Table 12: This table displays the employees who made more than the Mayor in the NYC payroll data.', options = list(pageLength = 15, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Position"), fontWeight = "bold", color = "#333333")

#Find total payroll expense if the Mayor's salary was the maximum salary for each `Fiscal Year`in 2014
#total_payroll_expense <- payroll_data_comp |>
  #filter(`Fiscal Year` == "2014") |>
  #left_join(total_savings, by = "Fiscal Year") |>
  #mutate(`Total Payroll Expense` = `Base Salary` + `Overtime Paid`) |>
  #group_by(`Fiscal Year`) |>
  #summarise(
  #`Total Payroll Expense` = max(`Total Savings`, na.rm = TRUE), .groups = "drop"
 # ) |>
 # arrange(`Fiscal Year`)
#View(total_payroll_expense)

# Policy # 2 Calculations:
# Find total number of overtime hours worked for each `Agency Name` and `Position` for each `Fiscal Year`
total_overtime_hours <- payroll_data_comp |>
  filter(`Overtime Hours` != 0) |>
  group_by(`Agency Name`, `Position`, `Fiscal Year`) |>
  summarise(
  `Total Overtime Hours` = sum(`Overtime Hours`, na.rm = TRUE), .groups = "drop"
  ) |>
  arrange(desc(`Fiscal Year`), desc(`Total Overtime Hours`))
#View(total_overtime_hours)
datatable(total_overtime_hours, caption = 'Table 13: This table displays the total overtime hours worked for each agency and position in the NYC payroll data.', options = list(pageLength = 15, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Position"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Total Overtime Hours"), fontWeight = "bold", color = "#333333")
# Find how many employees have `Pay Basis` == "per Annum" and `Overtime Hours` = 0
employees_per_annum <- payroll_data_comp |>
  filter(`Pay Basis` == "per Annum" & `Overtime Hours` == 0) |>
  group_by(`Fiscal Year`) |>
  summarise(
  `Total FT Employees` = n(), .groups = "drop"
  ) |>
  arrange(`Fiscal Year`)
View(employees_per_annum)
# and find how much employees have `Pay Basis` != "per Annum" and `Overtime Hours` > 0
employees_not_per_annum <- payroll_data_comp |>
  filter(`Pay Basis` != "per Annum" & `Overtime Hours` > 0) |>
  group_by(`Fiscal Year`) |>
  summarise(
  `Total PT Employees` = n(), .groups = "drop"
  ) |>
  arrange(`Fiscal Year`)  
View(employees_not_per_annum)
# 
# 
# Calculate total overtime hours and employee count per year
overtime_employee_data <- payroll_data_comp |>
  group_by(`Fiscal Year`) |>
  summarise(
    `Total Overtime Hours` = sum(`Overtime Hours`, na.rm = TRUE),
    `Employee Count` = n(),  # Total number of employees
    .groups = "drop"
  )
library(ggplot2)
library(scales)
library(tidyr)

overtime_employee_data_long <- overtime_employee_data |>
  pivot_longer(cols = c(`Total Overtime Hours`, `Employee Count`), 
               names_to = "Metric", 
               values_to = "Value")

ggplot(overtime_employee_data_long, aes(x = `Fiscal Year`, y = Value, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Overtime Hours and Employee Count by Fiscal Year",
       x = "Fiscal Year",
       y = "Count",
       color = "Metric") +
  facet_wrap(~ Metric, scales = "free_y") +  # Separate the plots and allow for different y-axis scales
  theme_minimal()
# 
correlation <- cor(overtime_employee_data$`Employee Count`, overtime_employee_data$`Total Overtime Hours`)
print(correlation)
# corr = 0.6179
# 
# 
# Find total savings if `Overtime Hours` == 0 for all employees with `Fiscal Year` == 2024
# Show the aggregate total_savings_2024 for each `Agency Name` and `Position`
total_savings_2024 <- payroll_data_comp |>
  filter(`Start Date` >= "2024-01-01", `Start Date` != "9999-12-31") |>
  mutate(`Total Savings` = `Overtime Hours` * (`Base Salary` - `Overtime Paid`)) |>
  group_by(`Fiscal Year`,`Agency Name`) |>
  summarise(
  `Total Savings` = sum(`Total Savings`, na.rm = TRUE), .groups = "drop"
  ) |>
  arrange(`Fiscal Year`)
View(total_savings_2024)

# Policy #3 Calculations:
# The total savings if overtime paid were reduced because the number of per hour employees was reduced.

total_savings_clean <- payroll_data_comp |>
  filter(!grepl("per Hour", `Pay Basis`)) |>
  mutate(`Overtime Paid` = ifelse(grepl("per Hour", `Pay Basis`), 0, `Overtime Paid`)) |>
  mutate(`Total Savings` = `Overtime Hours` * (`Base Salary` - `Overtime Paid`)) |>
  filter(`Total Savings` != 0) |>
  group_by(`Fiscal Year`, `Agency Name`) |>
  summarise(
    `Total Savings` = max(`Total Savings`, na.rm = TRUE), .groups = "drop"
  ) |>
  arrange(`Fiscal Year`)
View(total_savings_clean)

datatable(total_savings_clean, caption = 'Table 15: This table displays the total savings if overtime paid were reduced and the number of per hour employees was reduced.', options = list(pageLength = 15, autoWidth = TRUE),
            filter = "top", 
            extensions = c("Buttons", "Scroller"),
            class = "display",
            rownames = FALSE) |>
  formatCurrency(c("Total Savings"), currency = "$", interval = 3, mark = ",") |>
  formatStyle(c("Fiscal Year"), fontWeight = "bold", color = "#333333") |>
  formatStyle(c("Agency Name"), fontWeight = "bold", color = "#333333")|>
  formatStyle(c("Total Savings"), fontWeight = "bold", color = "#333333")
