# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(knitr)
library(kableExtra)

##Cleaning Data

# Specify the file path relative to the working directory
file_path <- "cdcdata-exercise/causeofdeathdata.csv"

# Load the CSV file into a data frame
cause_of_death_data_clean <- read.csv(file_path, stringsAsFactors = FALSE)

# Creating Number.Of.Days variable so that Start.Date and End.Date can be removed
# Month and Year are already variables, so Start.Date and End.Date become somewhat redundant
cause_of_death_data_clean$Number.Of.Days <- as.numeric(
  as.Date(cause_of_death_data_clean$End.Date, format = "%m/%d/%Y") - 
    as.Date(cause_of_death_data_clean$Start.Date, format = "%m/%d/%Y")
)
cause_of_death_data_clean <- cause_of_death_data_clean %>%
  dplyr::select(Jurisdiction.of.Occurrence, Year, Month, Number.Of.Days, everything())

# Removing variables which display only 'Data not shown (6 month lag)'
# Removing Start.Date, End.Date, and Data.As.Of variables
cause_of_death_data_clean <- subset(cause_of_death_data_clean, select = -c(flag_accid, flag_mva, flag_suic, flag_homic, flag_drugod,Start.Date,End.Date,Data.As.Of))

# Removing rows with any NA values
cause_of_death_data_clean <- cause_of_death_data_clean[complete.cases(cause_of_death_data_clean), ]

# Filtering out data from the year 2023 because it is incomplete
cause_of_death_data_clean <- cause_of_death_data_clean %>%
  filter(Year != 2023)

# Cleaning up variable names

clean_variable_names <- function(name) {
  name <- gsub("\\.+", " ", gsub("\\.\\.", "/", name))
  name <- gsub("Symptoms/Signs and Abnormal Clinical and Laboratory Findings/Not Elsewhere Classified", "Abnormal Findings (No Classifiable Diagnosis)", name)
  return(name)
}

cause_of_death_data_clean <- cause_of_death_data_clean %>%
  rename_with(clean_variable_names, everything())

## VISUALIZING DATA

# Calculate percentages total
cause_counts_total <- cause_of_death_data_clean %>%
  select(-c(`All Cause`, Year, Month, `Number Of Days`)) %>%
  gather(key = "Cause of Death", value = "count", -`Jurisdiction of Occurrence`) %>%
  group_by(`Cause of Death`) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  mutate(percentage = total_count / sum(total_count) * 100) %>%
  arrange(desc(total_count))

# Create pie chart 
pie_chart_total <- ggplot(cause_counts_total, aes(x = "", y = total_count, fill = `Cause of Death`)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Causes of Death",
       fill = "Cause of Death",
       x = NULL, y = NULL,
       caption = "Data source: CDC") +
  theme_void() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 10),  
        legend.key.size = unit(0.5, "lines"), 
        plot.title = element_text(size = 16),  
        plot.margin = margin(2, 6, 2, 2, "cm"),
        legend.box.margin = margin(0, -10, 0, 0)) +  
  guides(fill = guide_legend(
    keywidth = unit(0.5, "lines"),  
    label.position = "right",       
    label.hjust = 0                 
  )) +
  scale_fill_discrete(labels = paste0(cause_counts_total$`Cause of Death`, " (", round(cause_counts_total$percentage), "%)"))

# Adjustments
pie_chart_total <- pie_chart_total + theme(
  plot.margin = margin(2, 2, 2, 2, "cm"),
  plot.title = element_text(size = 16, hjust = 0.5, margin = margin(0, 0, 10, 0)),
  plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(10, 0, 0, 0))
)

# Show pie chart
print(pie_chart_total)

# Group by month and cause of death
cause_counts_month <- cause_of_death_data_clean %>%
  select(-c(`All Cause`, Year, `Number Of Days`)) %>%
  gather(key = "Cause of Death", value = "count", -`Jurisdiction of Occurrence`, -Month) %>%
  group_by(Month, `Cause of Death`) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  mutate(Month = factor(month.name[Month], levels = month.name)) %>%
  arrange(Month, desc(total_count))

# Create a stacked bar plot for causes of death by month
ggplot(cause_counts_month, aes(x = Month, y = total_count/1e6, fill = `Cause of Death`)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(format(x, big.mark = ",", scientific = FALSE), " million"), 
                     breaks = pretty_breaks()) + # format y-axis labels
  labs(title = "Total Count of Causes of Death by Month",
       x = "Month",
       y = "Total Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Calculate total deaths for each month
total_deaths_month <- cause_counts_month %>%
  group_by(Month) %>%
  summarise(total_deaths = sum(total_count))

# Calculate percentage of total deaths for each month
total_deaths_month <- total_deaths_month %>%
  mutate(percentage = total_deaths / sum(total_deaths) * 100)

# Plot the bar graph
ggplot(total_deaths_month, aes(x = Month, y = total_deaths/1e6, fill = Month)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(format(x, big.mark = ",", scientific = FALSE), " million"), 
                     breaks = pretty_breaks()) +
  labs(title = "Total Deaths by Month",
       x = "Month",
       y = "Total Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Print the table
kable(total_deaths_month, 
      col.names = c("Month", "Total Deaths", "Percentage"),
      format = "html",
      digits = 2,
      caption = "Total Deaths and Percentage by Month") %>%
  kable_styling(full_width = FALSE) %>%
  scroll_box(height = "200px")

# Group by year and cause of death
cause_counts_year <- cause_of_death_data_clean %>%
  select(-c(`All Cause`, Month, `Number Of Days`)) %>%
  gather(key = "Cause of Death", value = "count", -`Jurisdiction of Occurrence`, -Year) %>%
  group_by(Year, `Cause of Death`) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  arrange(Year, desc(total_count))

# Create a stacked bar plot for causes of death by month
ggplot(cause_counts_year, aes(x = Year, y = total_count/1e6, fill = `Cause of Death`)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(format(x, big.mark = ",", scientific = FALSE), " million"), 
                     breaks = pretty_breaks()) + # format y-axis labels
  labs(title = "Total Count of Causes of Death by Year",
       x = "Month",
       y = "Total Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Calculate total deaths for each year
total_deaths_year <- cause_counts_year %>%
  group_by(Year) %>%
  summarise(total_deaths = sum(total_count))

# Calculate percentage of total deaths for each year
total_deaths_year <- total_deaths_year %>%
  mutate(percentage = total_deaths / sum(total_deaths) * 100)

# Plot the bar graph
ggplot(total_deaths_year, aes(x = Year, y = total_deaths/1e6, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(format(x, big.mark = ",", scientific = FALSE), " million"), 
                     breaks = pretty_breaks()) +
  labs(title = "Total Deaths by Year",
       x = "Year",
       y = "Total Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Print the table
kable(total_deaths_year, 
      col.names = c("Year", "Total Deaths", "Percentage"),
      format = "html",
      digits = 2,
      caption = "Total Deaths and Percentage by Year") %>%
  kable_styling(full_width = FALSE) %>%
  scroll_box(height = "200px")

# Group by year, month, and cause of death
cause_counts_year_month <- cause_of_death_data_clean %>%
  select(-c(`All Cause`, `Number Of Days`)) %>%
  gather(key = "Cause of Death", value = "count", -`Jurisdiction of Occurrence`, -Year, -Month) %>%
  group_by(Year, Month, `Cause of Death`) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  arrange(Year, Month, desc(total_count))

# Create a stacked bar plot for causes of death by month and year
ggplot(cause_counts_year_month, aes(x = Month, y = total_count/1e6, fill = `Cause of Death`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year) +  # facet by year
  scale_y_continuous(labels = function(x) paste0(format(x, big.mark = ",", scientific = FALSE), " million"), 
                     breaks = pretty_breaks()) + # format y-axis labels
  labs(title = "Total Count of Causes of Death by Month and Year",
       x = "Month",
       y = "Total Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Calculate total deaths for each year and month
total_deaths_year_month <- cause_counts_year_month %>%
  group_by(Year, Month) %>%
  summarise(total_deaths = sum(total_count))

# Calculate percentage of total deaths for each year and month
total_deaths_year_month <- total_deaths_year_month %>%
  mutate(percentage = total_deaths / sum(total_deaths) * 100)

# Print the table
kable(total_deaths_year_month, 
      col.names = c("Year", "Month", "Total Deaths", "Percentage"),
      format = "html",
      digits = 2,
      caption = "Total Deaths and Percentage by Year and Month") %>%
  kable_styling(full_width = FALSE) %>%
  scroll_box(height = "200px")

