library(tidyverse)
library(fuzzyjoin)
library(stringr)
library(readr)
library(knitr)
library(kableExtra)
ai <- read_csv("ai_threat_index.csv")
bls <- read_csv("bls_projections.csv")
bls_clean <- bls |> 
  select(-`Occupation Code`, -`Median Annual Wage 2024`, -`Education Code`, -`Workex Code`)
#-Occupation Code, -Median Annual Wage 2023,Education Code, Workex Code
ai_clean <- ai |> 
  rename(job_title = `Job titiles`) |> 
  mutate(job_title = str_to_lower(str_trim(job_title)))

bls_clean <- bls_clean |> 
  rename(job_title = `Occupation Title`) |> 
  mutate(job_title = str_to_lower(str_trim(job_title)))
combined_data <- stringdist_inner_join(
  ai_clean, bls_clean,
  by = "job_title",
  max_dist = 0.4,         
  method = "jw"       
)
combined_data <- combined_data |> 
  select(-job_title.y)
combined_data <- combined_data |> 
  rename(job_title = job_title.x)
glimpse(combined_data)
view(combined_data)

#rename all columns in combined_data
combined_data <- combined_data |>
  select(-trCode) |>
  rename(
    Job_Title = job_title,
    AI_Impact_Ratio = `AI Impact`,
    Education_Level = `Typical Entry-Level Education`,
    Job_Training = `Typical on-the-job Training`,
    `Employment_Change_2023_2033` = `Employment Change, 2023-2033`,
    `Employment_Percent_Change_2023_2033` = `Employment Percent Change, 2023-2033`,
    `Work_Experience` = `Work Experience in a Related Occupation`,
    `Occupational_Openings_Annual_Avg` = `Occupational Openings, 2023-2033 Annual Average`
  )
names(combined_data)
# Use str_to_title() from the stringr package to capitalize the first letter of each word
combined_data$Job_Title <- stringr::str_to_title(combined_data$Job_Title)


# Verify the change by viewing the updated job titles
#head(combined_data$Job_Title)
#glimpse (combined_data)

glimpse_combined_data <- combined_data |> 
  slice_head(n = 20)
kable(glimpse_combined_data) |>
  scroll_box(width = "100%", height = "400px")

#what are the top 10 job titles with the highest AI impact ratios?
top_10_highAIImpact <- combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100
  ) |>
  arrange(desc(AI_Impact_Ratio)) |>
  slice_head(n = 10) |>
  select(Job_Title, AI_Impact_Ratio) |>
  rename("Job Title" = Job_Title, "AI Impact Ratio" = AI_Impact_Ratio)  # Renaming the column to "Job Title"
View(top_10_highAIImpact)
#create kable for the top 10 job titles with the highest AI impact ratios
top_10_highAIImpact |>
  kable() |>
  kable_styling(full_width = F, position = "left") |>
  column_spec(1, bold = T) |>
  column_spec(2, color = "black") |>
  row_spec(0, bold = T, background = "#D9EAD3") |>
  row_spec(1:10, background = "#F6F6F6") |>
  add_header_above(c("Top 10 Job Titles with Highest AI Impact Ratios" = 2)) |>
  kable_styling(latex_options = c("striped", "hold_position"))


#What are the top 10 job titles with the lowest AI impact ratios?
top_10_lowAIImpact <- combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100
  ) |>
  arrange(AI_Impact_Ratio) |>
  slice_head(n = 10) |>
  select(Job_Title, AI_Impact_Ratio) |>
  rename("Job Title" = Job_Title, "AI Impact Ratio" = AI_Impact_Ratio)  # Renaming the column to "Job Title"
View(top_10_lowAIImpact)
#create kable for the top 10 job titles with the lowest AI impact ratios
top_10_lowAIImpact |>
  kable() |>
  kable_styling(full_width = F, position = "left") |>
  column_spec(1, bold = T) |>
  column_spec(2, color = "black") |>
  row_spec(0, bold = T, background = "#D9EAD3") |>
  row_spec(1:10, background = "#F6F6F6") |>
  add_header_above(c("Top 10 Job Titles with Lowest AI Impact Ratios" = 2)) |>
  kable_styling(latex_options = c("striped", "hold_position"))

#what is the top ten job titles with the highest number of tasks?

top_10_highTaskCount <- combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(Tasks)) |>
  arrange(desc(Tasks)) |>
  slice_head(n = 10) |>
  select(Job_Title, Tasks) |>
  rename("Job Title" = Job_Title)  # Renaming the column
View(top_10_highTaskCount)

#create kable for the top 10 job titles with the highest number of tasks
top_10_highTaskCount |>
  kable() |>
  kable_styling(full_width = F, position = "left") |>
  column_spec(1, bold = T) |>
  column_spec(2, color = "black") |>
  row_spec(0, bold = T, background = "#D9EAD3") |>
  row_spec(1:10, background = "#F6F6F6") |>
  add_header_above(c("Top 10 Job Titles with Highest Number of Tasks" = 2)) |>
  kable_styling(latex_options = c("striped", "hold_position"))

#What is the top ten job titles with the lowest number of tasks?
top_10_lowTaskCount <- combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(Tasks)) |>
  arrange(Tasks) |>
  slice_head(n = 10) |>
  select(Job_Title, Tasks) |>
  rename("Job Title" = Job_Title)  # Renaming the column to "Job Title"
View(top_10_lowTaskCount)
#create kable for the top 10 job titles with the lowest number of tasks
top_10_lowTaskCount |>
  kable() |>
  kable_styling(full_width = F, position = "left") |>
  column_spec(1, bold = T) |>
  column_spec(2, color = "black") |>
  row_spec(0, bold = T, background = "#D9EAD3") |>
  row_spec(1:10, background = "#F6F6F6") |>
  add_header_above(c("Top 10 Job Titles with Lowest Number of Tasks" = 2)) |>
  kable_styling(latex_options = c("striped", "hold_position"))

#What is each education levels Average AI impact ratios? (from highest to lowest)
edulevel_avg_highAIImpact <- combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio), !is.na(Education_Level)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100,
    Education_Level = as.factor(Education_Level)
  ) |>
  group_by(Education_Level) |>
  summarise(
    Avg_AI_Impact_Ratio = round(mean(AI_Impact_Ratio, na.rm = TRUE), 3),
    Count = n()
  ) |>
  arrange(desc(Avg_AI_Impact_Ratio)) |>
  rename("Education Level" = Education_Level, 
         "Average AI Impact Ratio" = Avg_AI_Impact_Ratio)
print(edulevel_avg_highAIImpact)
View(edulevel_avg_highAIImpact)

#create kable for the average AI impact ratios by education level
edulevel_avg_highAIImpact |>
  kable() |>
  kable_styling(full_width = F, position = "left") |>
  column_spec(1, bold = T) |>
  column_spec(2, color = "black") |>
  row_spec(0, bold = T, background = "#D9EAD3") |>
  row_spec(1:8, background = "#F6F6F6") |>
  add_header_above(c("Average AI Impact Ratios by Education Level" = 2)) |>
  kable_styling(latex_options = c("striped", "hold_position"))

# # If you're looking at the average AI Impact Ratio across job titles:
# It summarizes the overall exposure of jobs to AI.
# 
# A higher average (e.g., 0.65 or 65%) means most jobs are more affected by AI.
# 
# A lower average (e.g., 0.25 or 25%) means AI impacts jobs less, on average.

#What is each education levels Maximum AI impact ratios? (from highest to lowest)
edulevel_max_highAIImpact <- combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio), !is.na(Education_Level)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100,
    Education_Level = as.factor(Education_Level)
  ) |>
  group_by(Education_Level) |>
  summarise(
    Max_AI_Impact_Ratio = round(max(AI_Impact_Ratio, na.rm = TRUE), 3),
    Count = n()
  ) |>
  arrange(desc(Max_AI_Impact_Ratio)) |>
  rename("Education Level" = Education_Level, 
         "Max AI Impact Ratio" = Max_AI_Impact_Ratio)
print(edulevel_max_highAIImpact)
View(edulevel_max_highAIImpact)

#create kable for the maximum AI impact ratios by education level
edulevel_max_highAIImpact |>
  kable() |>
  kable_styling(full_width = F, position = "left") |>
  column_spec(1, bold = T) |>
  column_spec(2, color = "black") |>
  row_spec(0, bold = T, background = "#D9EAD3") |>
  row_spec(1:8, background = "#F6F6F6") |>
  add_header_above(c("Maximum AI Impact Ratios by Education Level" = 2)) |>
  kable_styling(latex_options = c("striped", "hold_position"))
# The job titles associated with higher education levels (like Bachelor's degree and Doctoral degree) tend to have higher AI impact ratios, but interestingly, there are some jobs with a High school diploma or No formal educational credential that also have very high AI impact ratios.
# 
# The Count tells you how many job titles within each education level contribute to the maximum AI impact ratio. For example, there are 2233 job titles with a Bachelor's degree, and the maximum AI impact ratio among those job titles is 0.98.
#library(ggplot2)

# Create a bar plot for Max AI Impact Ratio by Education Level
ggplot(edulevel_max_highAIImpact, aes(x = `Education Level`, y = `Max AI Impact Ratio`, fill = `Education Level`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Maximum AI Impact Ratio by Education Level",
    x = "Education Level",
    y = "Max AI Impact Ratio"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black")) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(0, 1, 0.1)) +
  geom_text(aes(label = Count), vjust = -0.5, size = 3.5) +
  theme(legend.position = "none")



################################################################################

#What is the distribution of AI impact ratios across different job titles?
combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100
  ) |>
   ggplot(aes(x = AI_Impact_Ratio)) +
  geom_histogram(bins = 30, fill="darkblue", color = "black", alpha = 0.7) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    title = "Distribution of AI Impact Ratio",
    x = "AI Impact Ratio",
    y = "Job Count"
  ) +
  theme_minimal()+
  theme(
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text = element_text(size = 10, face="bold", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
        legend.position = "none",
        plot.title.position = "plot"
        )

#What is the distribution of AI impact ratios across different education levels?
combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio), !is.na(Education_Level)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100,
    Education_Level = as.factor(Education_Level)
  ) |>
  ggplot(aes(x = Education_Level, y = AI_Impact_Ratio, fill = Education_Level)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = scales::percent_format()
  ) +
  labs(
    title = "Distribution of AI Impact Ratio by Education Level",
    x = "Education Level",
    y = "AI Impact Ratio"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold", color = "black"),
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text = element_text(size = 10, face="bold", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black")
        ) # Rotate + resize labels)

#
#What is the relationship between AI impact ratios and the number of tasks performed?
combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio), !is.na(Tasks)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100
  ) |>
  ggplot(aes(x = Tasks, y = AI_Impact_Ratio)) +
  geom_point(alpha = 0.7, color = "deepskyblue2", size = 2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    title = "AI Impact Ratio vs Number of Tasks",
    x = "Number of Tasks",
    y = "AI Impact Ratio",
  ) +
  theme_minimal()+
  theme(
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text = element_text(size = 10, face="bold", color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold", color = "black"),
        legend.position = "none",
        plot.title.position = "plot"
        )

#What is the relationship between AI impact ratios and the number of tasks performed across different education levels?
combined_data |>
  distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio), !is.na(Tasks), !is.na(Education_Level)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100,
    Education_Level = as.factor(Education_Level)
  ) |>
  ggplot(aes(x = Tasks, y = AI_Impact_Ratio)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  facet_wrap(~ Education_Level, scales = "free_x") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    title = "AI Impact Ratio vs Number of Tasks by Education Level",
    x = "Number of Tasks",
    y = "AI Impact Ratio"
  ) +
  theme_minimal()+
  theme(
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text = element_text(size = 10, face="bold", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
        strip.text = element_text(size = 8, face = "bold", color = "black"),
        legend.position = "none",
        plot.title.position = "plot"
        )



#How does the AI impact ratio correlate with the number of tasks performed and the education level associated with different job titles?

library(ggplot2)
library(plotly)

combined_data |>
  #distinct(Job_Title, .keep_all = TRUE) |>
  filter(!is.na(AI_Impact_Ratio), !is.na(Tasks), !is.na(Education_Level)) |>
  mutate(
    AI_Impact_Ratio = as.numeric(str_remove(AI_Impact_Ratio, "%")) / 100,
    Education_Level = as.factor(Education_Level)
  ) |>
  ggplot(aes(x = Tasks, y = AI_Impact_Ratio)) +
  geom_bin2d(bins = c(30, 20)) +
  facet_wrap(~ Education_Level, scales = "free_x") +
  scale_fill_gradient(
    low = "darkblue", high = "darkorange", name = "Job Count"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    title = "AI Impact vs Tasks by Education Level",
    x = "Number of Tasks",
    y = "AI Impact Ratio"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "darkblue", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),
    axis.title.y = element_text(size = 16, face = "bold", color = "darkblue"),
    axis.text = element_text(size = 9, face ="bold", color = "black"),
    strip.text = element_text(size = 9, face = "bold", color = "black"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.position = "right",
    plot.title.position = "plot"
  ) -> plot

# Convert to interactive plotly object
interactive_plot <- ggplotly(plot)

# Show interactive plot
interactive_plot

#The AI Impact Ratio is the percentage representation of AI's influence on the respective job title.
# converts this percentage string (e.g., "75%") to a decimal (0.75) for plotting.
# Y-axis in the plot, showing how strongly AI affects different jobs
#
#The Tasks is the count of tasks associated with the job title.
#Plotted on the X-axis, it helps visualize whether jobs with more or fewer tasks are more impacted by AI.
#
#Typical education needed for entry. Occupations are assigned one of the following eight education levels:
# converts it to a factor and uses it in facet_wrap() to create a separate mini-plot for each level.
#This allows for easy comparison between, say, "High school diploma" and "Doctoral degree" jobs.
#
#Doctoral or professional degree. Completion of a doctoral degree (Ph.D.) or professional degree usually requires at least 3 years of full-time academic work beyond a bachelor's degree.
# Master's degree. Completion of this degree usually requires 1 or 2 years of full-time academic study beyond a bachelor's degree.
# Bachelor's degree. Completion of this degree generally requires at least 4 years, but not more than 5 years, of full-time academic study beyond high school.
# Associate's degree. Completion of this degree usually requires at least 2 years but not more than 4 years of full-time academic study beyond high school.
# Post secondary non degree award. These programs lead to a certificate or other award, but not a degree. The certificate is awarded by the educational institution and is the result of completing formal post secondary schooling. Certification, issued by a professional organization or certifying body, is not included here. Some postsecondary nondegree award programs last only a few weeks, while others may last 1 to 2 years.
# Some college, no degree. This category signifies the achievement of a high school diploma or equivalent plus the completion of one or more postsecondary courses that did not result in a degree or award.
# High school diploma or equivalent. This category indicates the completion of high school or an equivalent program resulting in the award of a high school diploma or an equivalent, such as the General Education Development (GED) credential.
# No formal educational credential. This category signifies that a formal credential issued by an educational institution, such as a high school diploma or post secondary certificate, is not typically needed for entry into the occupation.
#
#
# What the graph says?
# The graph shows the relationship between AI impact ratios and the number of tasks performed across different education levels.
# The orange color indicates a higher job count, while the blue color indicates a lower job count.
# Which means that the less task a jobs and the more jobs are available in that titles the more likely it it to be 
# The less task the higher the AI impact ratio, meaning that AI is more likely to take over jobs with fewer tasks.
# The High school diploma and Bachelor degree education levels were more likely to have a higher job count and lower number of tasks, despite also having a low AI impact ratio, 
# meaning that job titles that possess with those education levels are less likely to be taken over by AI. The other degree with the higher job counts that had this phenomenon is the Doctoral or professional degrees
#The graph also shows that the lower the job task count, the higher the AI impact ratio, meaning that jobs with fewer tasks are more likely to be taken over by AI.
#The jobs with lower tasks and higher AI impact ratio are more likely to be taken over by AI.
#The jobs with higher task count has a lower AI impact ratio, meaning that jobs with more tasks are less likely to be taken over by AI.
#All the graphs show that there is no correlation between high AI impact and high number of tasks, meaning that the more tasks a job has, the less likely it is to be taken over by AI.
# The education level with the least amount of data points was some college no degree
# The one with the most data points is the Bachelor's Degree
# Only the Master's degree and the No formal education credential education level had no data point at 100% AI Impact ratio.
# Most of the data points were concentrated between the 0% and 50% AI impact ratio and the 0 to 1000 number of tasks, meaning the job titles of all education levels were similarity likely to be taken over by A with the max chance being 50% even with a high number of tasks.
# What we can conclude from this?
# Most jobs are not going to be taken over by AI, even with a low or high number of tasks.
# Therefore, a fewer group of job titles need to be concerned about their job security.
# However, you are more at risk when the job count is higher and the job has a  lower number of tasks, especially with job holders that have have a education level of high school, Bachelors or Doctoral degree.
# 
# How it relates to the overarching question?
#The over arching question is how will jobs be affected by AI. This specific question answer this by looking at how education level, task count and AI impact ratio are related to each other.
#The graph shows that the more tasks a job has, the less likely it is to be taken over by AI.
#The jobs titles that will be affected is those with a lower number of tasks and a higher job count, especially the following education levels: High school diploma, Bachelor's degree and Doctoral or professional degrees.


