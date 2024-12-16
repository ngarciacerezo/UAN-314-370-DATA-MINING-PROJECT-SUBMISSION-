# Import data


library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# Import data
activity <- read.csv("/Users/pablosaez/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
calories <- read.csv("/Users/pablosaez/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
intensities <- read.csv("/Users/pablosaez/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
sleep <- read.csv("/Users/pablosaez/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight <- read.csv("/Users/pablosaez/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
heart <- read.csv("/Users/pablosaez/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
Steps <- read.csv("/Users/pablosaez/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")

#We want to merge three of the dataset, Activity, Sleep, and Weight. For that we are going to change the names of the variables first.
# Load the dplyr library
library(dplyr)

# Rename the SleepDay column to date in the sleep dataset
sleep <- sleep %>%
  rename(Date = date)

# Rename the ActivityDate column to date in the activity dataset
activity <- activity %>%
  rename(Date = ActivityDate)

# Convert datetime to just date
sleep$Date <- as.Date(sleep$Date)

# We were having some probelms with the format of the data and we found this way to improve the study
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
activity$Date=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
# weight
weight$Date=as.POSIXct(weight$Date, format="%m/%d/%Y", tz=Sys.timezone())
weight$date <- format(weight$Date, format = "%m/%d/%y")

#We wanted to count how many unique persons there were in each dataset

n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

# Install lubridate if not already installed
install.packages("lubridate")
library(lubridate)

# Convert SleepDay to date-only format
sleep$SleepDay <- ymd_hms(sleep$SleepDay) %>% as.Date()

# Merge sleep and activity datasets by ID
merged_data <- merge(sleep, activity, by = "Id")


#After these we realized that even knwing we have a lot of observations most of them are based on the same individuals since for example in activity there is only 33 unique individuals

# activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

# explore num of active minutes per category
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# calories
calories %>%
  select(Calories) %>%
  summary()
# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weight %>%
  select(WeightKg, BMI) %>%
  summary()


#---------------------------------------------------------------------------------

# Merge datasets by "Id"
Activity_Calories <- merge(activity, calories, by = "Id")

# View the merged dataset
print(Activity_Calories)

summary(Activity_Calories)

#Cleaning data

# Check for missing values in each column
colSums(is.na(Activity_Calories))

# Get a summary of all variables. With this we can estimate based on the max, the average and our logic what variables mught have outliers.
summary(Activity_Calories) 

# Example of data sampling: Randomly sample 10% of the data
sampled_Act.Cal <- Activity_Calories %>%
  sample_frac(0.005)

lm_model <- lm(Calories.y ~ TotalSteps + VeryActiveDistance + LightActiveDistance, data = sampled_Act.Cal)
summary(lm_model)


# Calculate IQR for Calories.y
Q1 <- quantile(sampled_Act.Cal$Calories.y, 0.25)  # First quartile
Q3 <- quantile(sampled_Act.Cal$Calories.y, 0.75)  # Third quartile
IQR <- Q3 - Q1  # Interquartile range

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter data to remove outliers
cleaned_Act_CAL <- sampled_Act.Cal %>%
  filter(Calories.y >= lower_bound & Calories.y <= upper_bound)

# Check the cleaned data
summary(cleaned_Act_CAL)

lm_model <- lm(Calories ~ TotalSteps + VeryActiveDistance + LightActiveDistance + TotalSteps, data = activity)
summary(lm_model)

#---------------------------------LINEAR REGRESSIONS-------------------------------------------------------------------

#Regression 1_ Activity
# Linear Regression Model for Calories Burned
activity_lm <- lm(Calories ~ TotalSteps + TotalDistance + VeryActiveMinutes + 
                    LightlyActiveMinutes + SedentaryMinutes, data = activity)

# Summary of the model
summary(activity_lm)

#Regression 2_Sleep
# Calculate Sleep Efficiency
sleep <- sleep %>%
  mutate(SleepEfficiency = TotalMinutesAsleep / TotalTimeInBed)

# Linear Regression Model for Sleep Efficiency
sleep_lm <- lm(SleepEfficiency ~ TotalSleepRecords + TotalTimeInBed, data = sleep)

# Summary of the model
summary(sleep_lm)


# Regression 3_ Weight_ Doesn't Work
# Linear Regression Model for BMI
weight_lm <- lm(BMI ~ WeightKg + Fat, data = weight)

# Summary of the model
summary(weight_lm)

#Regression 4_ 
# Merge activity and sleep datasets
activity_sleep <- merge(activity, sleep, by.x = c("Id", "ActivityDate"), by.y = c("Id", "SleepDay"))

# Calculate Sleep Efficiency
activity_sleep <- activity_sleep %>%
  mutate(SleepEfficiency = TotalMinutesAsleep / TotalTimeInBed)

# Linear Regression Model for Calories Burned
activity_sleep_lm <- lm(Calories ~ TotalSteps + VeryActiveMinutes + SleepEfficiency, data = activity_sleep)

# Summary of the model
summary(activity_sleep_lm)

#-------------------------ACTIVITY DATASET-----------------------------------------------------------------

#PLOT 1:Acitivity Distribution
# Load necessary library
library(ggplot2)

# Sumar las columnas relevantes en el dataset llamado 'activity'
activity_distribution <- colSums(activity[, c("VeryActiveMinutes", "FairlyActiveMinutes", 
                                              "LightlyActiveMinutes", "SedentaryMinutes")])

# Convertir a un data frame para usar con ggplot
activity_distribution_df <- data.frame(
  ActivityType = names(activity_distribution),
  TotalMinutes = as.numeric(activity_distribution)
)

# Cargar la librería necesaria
library(ggplot2)

# Crear el gráfico de barras
ggplot(activity_distribution_df, aes(x = ActivityType, y = TotalMinutes)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Total Minutes dedicated in each Activity",
       x = "Activity Type",
       y = "Total Minutes") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#PLOT 2: Steps Trend Over Time
# Load necessary library
library(ggplot2)

# Create a smooth trend line using LOESS
ggplot(activity, aes(x = ActivityDate, y = TotalSteps)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Smoothed Trend of Total Steps Over Time",
       x = "Date",
       y = "Total Steps") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#PLOT 3: Total Steps vs. Calories

ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")

#PLOT 4: Active Minutes vs. Calories
# Create a new column for Total Active Minutes
activity$TotalActiveMinutes <- activity$VeryActiveMinutes + 
  activity$FairlyActiveMinutes + 
  activity$LightlyActiveMinutes

#PLOT 5:  Calories Burned vs. Active Minutes

# Calculate total active minutes if not already present
activity$TotalActiveMinutes <- activity$VeryActiveMinutes + 
  activity$FairlyActiveMinutes + 
  activity$LightlyActiveMinutes

# Enhanced scatter plot: Calories Burned vs. Total Active Minutes
ggplot(activity, aes(x = TotalActiveMinutes, y = Calories)) +
  geom_point(aes(color = SedentaryMinutes, size = LightlyActiveMinutes), alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "dotted") +
  theme_minimal() +
  labs(title = "Calories Burned vs. Total Active Minutes",
       subtitle = "Point size represents Lightly Active Minutes; Color represents Sedentary Minutes",
       x = "Total Active Minutes",
       y = "Calories Burned",
       color = "Sedentary Minutes",
       size = "Lightly Active Minutes") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#PLOT 6: This is plot 4 enhance.

# Enhanced scatter plot: Calories Burned vs. Total Steps
ggplot(activity, aes(x = TotalSteps, y = Calories)) +
  geom_point(aes(color = TotalDistance, size = VeryActiveMinutes), alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Calories Burned vs. Total Steps",
       subtitle = "Point size represents Very Active Minutes; Color represents Total Distance",
       x = "Total Steps",
       y = "Calories Burned",
       color = "Total Distance",
       size = "Very Active Minutes") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#PLOT 7: Distance Breakdown by Acitvity levels
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Prepare data: Summarize distance by activity type
distance_breakdown <- activity %>%
  select(VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, SedentaryActiveDistance) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  pivot_longer(cols = everything(), names_to = "ActivityType", values_to = "Distance")

# Create the stacked bar plot
ggplot(distance_breakdown, aes(x = "", y = Distance, fill = ActivityType)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) + # For circular (pie-chart-like) presentation
  theme_minimal() +
  labs(title = "Distance Breakdown by Activity Levels",
       x = NULL,
       y = "Total Distance",
       fill = "Activity Type") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

#PLOT 8: Correlation Heat
# Install reshape2
install.packages("reshape2")

# Install viridis
install.packages("viridis")
# Load necessary libraries
library(ggplot2)
library(reshape2)
library(viridis) # For a visually appealing color palette

# Select relevant numeric columns for correlation
correlation_data <- activity[, c("TotalSteps", "TotalDistance", "VeryActiveDistance", 
                                 "ModeratelyActiveDistance", "LightActiveDistance", 
                                 "SedentaryActiveDistance", "Calories")]

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")

# Convert the matrix into a long format for ggplot
correlation_long <- melt(correlation_matrix)

# Create the heatmap
ggplot(correlation_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Correlation", option = "plasma", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Heatmap",
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10),
        panel.grid = element_blank())

#PLOT 9: Average Sedentary Time vs. Total Steps; This can show whether prolonged sedentary behavior impacts physical activity.

avg_sedentary_steps <- activity %>%
  group_by(SedentaryMinutes) %>%
  summarise(AverageSteps = mean(TotalSteps, na.rm = TRUE))

ggplot(avg_sedentary_steps, aes(x = SedentaryMinutes, y = AverageSteps)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred") +
  labs(
    title = "Average Steps vs. Sedentary Minutes",
    x = "Sedentary Minutes",
    y = "Average Steps"
  ) +
  theme_minimal()

#PLOT 10:
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure date column is in proper format
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")

# Filter data for TotalSteps < 50 and add day of the week
filtered_activity <- activity %>%
  filter(TotalSteps < 50) %>%
  mutate(DayOfWeek = weekdays(ActivityDate))

# Summarize average calories burned per day of the week
calories_by_day <- filtered_activity %>%
  group_by(DayOfWeek) %>%
  summarise(AverageCalories = mean(Calories, na.rm = TRUE))

# Reorder days of the week to follow standard order
calories_by_day$DayOfWeek <- factor(calories_by_day$DayOfWeek, 
                                    levels = c("Monday", "Tuesday", "Wednesday", 
                                               "Thursday", "Friday", "Saturday", "Sunday"))

# Create bar plot
ggplot(calories_by_day, aes(x = DayOfWeek, y = AverageCalories)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Average Calories Burned per Day (Steps < 50)",
       x = "Day of the Week",
       y = "Average Calories Burned") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-----------------------------------MERGES NATALIA-------------------------------------

# PLOT 1: Calories Burned by Hourly Intensity;Understand how higher intensity translates into more calories burned.
intensity_calories <- merge(intensities, calories, by = c("Id", "ActivityHour"))

ggplot(intensity_calories, aes(x = TotalIntensity, y = Calories)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Calories Burned vs. Total Intensity",
    x = "Total Intensity",
    y = "Calories Burned"
  ) +
  theme_minimal()

# PLOT 2: Comparison of Active and Sedentary Time Across Weekdays; Shows how daily activity patterns vary by day of the week
activity$DayOfWeek <- weekdays(activity$ActivityDate)

avg_activity_weekday <- activity %>%
  group_by(DayOfWeek) %>%
  summarise(
    AvgActiveMinutes = mean(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes, na.rm = TRUE),
    AvgSedentaryMinutes = mean(SedentaryMinutes, na.rm = TRUE)
  )

ggplot(avg_activity_weekday, aes(x = DayOfWeek)) +
  geom_bar(aes(y = AvgActiveMinutes, fill = "Active Minutes"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = AvgSedentaryMinutes, fill = "Sedentary Minutes"), stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Active and Sedentary Time Across Weekdays",
    x = "Day of Week",
    y = "Minutes",
    fill = "Activity Type"
  ) +
  theme_minimal()

#PLOT 3: Scatterplot of Total Steps vs. BMI; Analyzes whether BMI affects physical activity levels.
activity_weight <- merge(activity, weight, by = "Id")

ggplot(activity_weight, aes(x = BMI, y = TotalSteps)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "Relationship Between BMI and Total Steps",
    x = "BMI",
    y = "Total Steps"
  ) +
  theme_minimal()

#PLOT 4: #Average Steps by Day of the Week (Bar Chart);Highlights patterns of physical activity across the week, which can inform activity planning.
activity$DayOfWeek <- weekdays(activity$ActivityDate)

avg_steps_weekday <- activity %>%
  group_by(DayOfWeek) %>%
  summarise(AverageSteps = mean(TotalSteps, na.rm = TRUE))

ggplot(avg_steps_weekday, aes(x = reorder(DayOfWeek, AverageSteps), y = AverageSteps, fill = DayOfWeek)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Steps by Day of the Week",
    x = "Day of Week",
    y = "Average Steps"
  ) +
  theme_minimal()

#PLOT 5: 
#This visualizes how calorie burn increases with active time.
#Calories Burned Over Duration of Active Time (Line Chart);Shows how calories burned correlate with total active minutes. This is useful for understanding fitness efficiency.
activity <- activity %>%
  mutate(ActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes)

ggplot(activity, aes(x = ActiveMinutes, y = Calories)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(
    title = "Calories Burned Over Active Time",
    x = "Active Minutes",
    y = "Calories Burned"
  ) +
  theme_minimal()

#PLOT 6: 

#-----------------------------------------Activity & Sleep---------------------------------------------------

#PLOT 1: 
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure date columns are properly formatted
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

# Merge the datasets on 'Id' and date
merged_data <- merge(activity, sleep, by.x = c("Id", "ActivityDate"), by.y = c("Id", "SleepDay"))

# Calculate Sleep Efficiency
merged_data <- merged_data %>%
  mutate(SleepEfficiency = TotalMinutesAsleep / TotalTimeInBed)

# Scatter plot: Total Steps vs Total Minutes Asleep
ggplot(merged_data, aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_point(aes(color = SleepEfficiency, size = VeryActiveMinutes), alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") + # For a beautiful color palette
  theme_minimal() +
  labs(title = "Activity vs Sleep Quality",
       subtitle = "Size represents Active Minutes, Color represents Sleep Efficiency",
       x = "Total Steps",
       y = "Total Minutes Asleep",
       color = "Sleep Efficiency",
       size = "Active Minutes") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#PLOT 2: 
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure date columns are properly formatted
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

# Calculate Sleep Efficiency
sleep <- sleep %>%
  mutate(SleepEfficiency = TotalMinutesAsleep / TotalTimeInBed)

# Line plot: Sleep Efficiency over Time, grouped by User (Id)
ggplot(sleep, aes(x = SleepDay, y = SleepEfficiency, group = Id, color = as.factor(Id))) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Sleep Efficiency Over Time",
       subtitle = "Grouped by User (Id)",
       x = "Date",
       y = "Sleep Efficiency",
       color = "User Id") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#PLOT 3:
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure the date columns are properly formatted
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

# Merge the datasets on 'Id' and date
merged_data <- merge(activity, sleep, by.x = c("Id", "ActivityDate"), by.y = c("Id", "SleepDay"))

# Calculate Sleep Efficiency
merged_data <- merged_data %>%
  mutate(SleepEfficiency = TotalMinutesAsleep / TotalTimeInBed)

# Scatter plot: Calories Burned vs. Total Minutes Asleep
ggplot(merged_data, aes(x = TotalMinutesAsleep, y = Calories)) +
  geom_point(aes(color = SleepEfficiency, size = TotalSteps), alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Calories Burned vs. Sleep Duration",
       subtitle = "Point size represents Total Steps, Color represents Sleep Efficiency",
       x = "Total Minutes Asleep",
       y = "Calories Burned",
       color = "Sleep Efficiency",
       size = "Total Steps") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#PLOT 4:
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure the date columns are properly formatted
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

# Merge the datasets on 'Id' and date
merged_data <- merge(activity, sleep, by.x = c("Id", "ActivityDate"), by.y = c("Id", "SleepDay"))

# Scatter plot: Total Distance vs. Total Minutes Asleep
ggplot(merged_data, aes(x = TotalDistance, y = TotalMinutesAsleep)) +
  geom_point(aes(color = VeryActiveMinutes, size = Calories), alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Total Distance vs. Sleep Duration",
       subtitle = "Color represents Very Active Minutes, Size represents Calories Burned",
       x = "Total Distance (miles)",
       y = "Total Minutes Asleep",
       color = "Very Active Minutes",
       size = "Calories Burned") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


#PLOT 5: 
# Create a categorical variable for distance ranges
merged_data <- merged_data %>%
  mutate(DistanceCategory = cut(TotalDistance, breaks = c(0, 2, 5, 8, Inf), 
                                labels = c("0-2 miles", "2-5 miles", "5-8 miles", ">8 miles")))

# Boxplot: Total Minutes Asleep for different distance categories
ggplot(merged_data, aes(x = DistanceCategory, y = TotalMinutesAsleep)) +
  geom_boxplot(aes(fill = DistanceCategory)) +
  theme_minimal() +
  labs(title = "Sleep Duration by Distance Categories",
       x = "Distance Categories",
       y = "Total Minutes Asleep",
       fill = "Distance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------SLEEP & WEIGHT----------------------------------------------------------

#PLOT 1:
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure date columns are in proper date format
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")
weight$Date <- as.Date(weight$Date, format = "%m/%d/%Y")

# Merge the datasets on 'Id' and 'Date'
merged_sleep_weight <- merge(sleep, weight, by.x = c("Id", "SleepDay"), by.y = c("Id", "Date"))

# Filter data for weights between 60 and 80 kg
filtered_data <- merged_sleep_weight %>%
  filter(WeightKg >= 55 & WeightKg <= 65)

# Create a scatter plot with restricted weight range
ggplot(filtered_data, aes(x = WeightKg, y = TotalMinutesAsleep)) +
  geom_point(aes(color = BMI, size = TotalTimeInBed), alpha = 0.7) +
  scale_color_viridis_c(option = "magma") +
  theme_minimal() +
  labs(title = "Weight (60–80 kg) vs Sleep Duration",
       subtitle = "Point size represents Time in Bed, Color represents BMI",
       x = "Weight (kg)",
       y = "Total Minutes Asleep",
       color = "BMI",
       size = "Total Time in Bed") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


#PLOT 2: 
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Calculate sleep efficiency
merged_sleep_weight <- merged_sleep_weight %>%
  mutate(SleepEfficiency = TotalMinutesAsleep / TotalTimeInBed)

# Filter outliers: Only include valid sleep efficiency and BMI ranges
filtered_data <- merged_sleep_weight %>%
  filter(SleepEfficiency >= 0.5 & SleepEfficiency <= 1, 
         BMI >= 15 & BMI <= 40) # Adjust BMI range as needed

# Scatter plot: BMI vs Sleep Efficiency with outlier removal and trend line
ggplot(filtered_data, aes(x = BMI, y = SleepEfficiency)) +
  geom_point(aes(color = WeightKg, size = TotalMinutesAsleep), alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "BMI vs Sleep Efficiency",
       subtitle = "Point size represents Sleep Duration, Color represents Weight (kg)",
       x = "BMI",
       y = "Sleep Efficiency",
       color = "Weight (kg)",
       size = "Sleep Duration") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#----------------------------------------------QUERIES---------------------------------------
 #--------------------------------------------Activity--------------------------------------

#QUERY1. Activity Level Analysis --> this oculd be relevant to deduce what days of the week people used to loose weight even when doing nothing.
#Query: Which days have the highest and lowest total steps or distances covered?
#Why? This provides insights into the most and least active days, helping to identify trends or patterns in activity levels over time.

#Query 1.1
# Query for the day with the highest total steps
highest_steps <- sqldf("SELECT * 
                        FROM activity
                        WHERE TotalSteps = (SELECT MAX(TotalSteps) FROM activity)")
# Query for the top 5 days with the highest total steps
top_5_steps <- sqldf("SELECT * 
                      FROM activity
                      ORDER BY TotalSteps DESC
                      LIMIT 5")
# Query for the day with the lowest total steps
lowest_steps <- sqldf("SELECT * 
                       FROM activity
                       WHERE TotalSteps = (SELECT MIN(TotalSteps) FROM activity)")

# Print the results
print("Day with the highest total steps:")
print(highest_steps)

# Print the results
print("Top 5 days of highest steps")
print(top_5_steps)


print("Day with the lowest total steps:")
print(lowest_steps)

#Query 1.2
# Query for the top 5 days with the highest total distance
top_5_distance <- sqldf("SELECT * 
                         FROM activity
                         ORDER BY TotalDistance DESC
                         LIMIT 5")

# Query for the day with the lowest total distance
lowest_distance <- sqldf("SELECT * 
                          FROM activity
                          WHERE TotalDistance = (SELECT MIN(TotalDistance) FROM activity)")

# Print the results
print("Top 5 days highes distance:")
print(top_5_distance)

print("Day with the lowest total distance:")
print(lowest_distance)

#QUERY2: 2. Calories vs. Activity
#Query: How do total steps or active minutes correlate with calories burned?
#Why? Understanding this relationship can highlight how much activity is needed to achieve calorie-burning goals.

# Query to get relevant data
activity_data <- sqldf("SELECT TotalSteps, VeryActiveMinutes, LightlyActiveMinutes, Calories 
                        FROM activity")

# Correlation between TotalSteps and Calories
cor_steps_calories <- cor(activity_data$TotalSteps, activity_data$Calories, use = "complete.obs")

# Correlation between VeryActiveMinutes and Calories
cor_veryactive_calories <- cor(activity_data$VeryActiveMinutes, activity_data$Calories, use = "complete.obs")

# Correlation between LightlyActiveMinutes and Calories
cor_lightactive_calories <- cor(activity_data$LightlyActiveMinutes, activity_data$Calories, use = "complete.obs")

# Print correlations
print(paste("Correlation between Total Steps and Calories:", cor_steps_calories))
print(paste("Correlation between Very Active Minutes and Calories:", cor_veryactive_calories))
print(paste("Correlation between Lightly Active Minutes and Calories:", cor_lightactive_calories))

#PLOT $ This plots could actually be used for the visualization part.
# Scatter plot: Total Steps vs. Calories
ggplot(activity_data, aes(x = TotalSteps, y = Calories)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Total Steps vs. Calories Burned", x = "Total Steps", y = "Calories Burned")

# Scatter plot: Very Active Minutes vs. Calories
ggplot(activity_data, aes(x = VeryActiveMinutes, y = Calories)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Very Active Minutes vs. Calories Burned", x = "Very Active Minutes", y = "Calories Burned")

# Scatter plot: Lightly Active Minutes vs. Calories
ggplot(activity_data, aes(x = LightlyActiveMinutes, y = Calories)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Lightly Active Minutes vs. Calories Burned", x = "Lightly Active Minutes", y = "Calories Burned")

#THE EXPLANATION COULD BE THAT AFTER CALCULATING THE CORRELATINO WE WANTED TO TEST WITH A VISUALIZATION

#QUERY 3: 

# Install and load sqldf
if (!require(sqldf)) {
  install.packages("sqldf")
}
library(sqldf)

# Calculate sedentary and active time percentages
sedentary_active <- sqldf("
  SELECT *,
         (SedentaryMinutes / 1440.0) * 100 AS SedentaryPercentage,
         ((VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes) / 1440.0) * 100 AS ActivePercentage
  FROM activity
")

# View the result
head(sedentary_active)

# Calculate sedentary and active time percentages
sedentary_active <- activity %>%
  mutate(SedentaryPercentage = (SedentaryMinutes / 1440) * 100,
         ActivePercentage = ((VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes) / 1440) * 100)

# View the result
head(sedentary_active)

#QUERY 4: 
# Install and load sqldf
if (!require(sqldf)) {
  install.packages("sqldf")
}
library(sqldf)

# Query to calculate total distances covered in different intensities across days
activity_intensity <- sqldf("
  SELECT ActivityDate, 
         SUM(VeryActiveDistance) AS TotalVeryActiveDistance,
         SUM(ModeratelyActiveDistance) AS TotalModeratelyActiveDistance,
         SUM(LightActiveDistance) AS TotalLightActiveDistance
  FROM activity
  GROUP BY ActivityDate
")

# View the summarized data
head(activity_intensity)

  #-----------------------------ACTIVITY & SLEEP-------------------------------

#First we convert everything 
# Convert dates to Date type
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

# Merge datasets on Id and Date
merged_data <- sqldf("
  SELECT a.*, s.TotalSleepRecords, s.TotalMinutesAsleep, s.TotalTimeInBed
  FROM activity a
  LEFT JOIN sleep s
  ON a.Id = s.Id AND a.ActivityDate = s.SleepDay
")

#Query1
# Query to analyze activity vs sleep quality
activity_sleep_analysis <- sqldf("
  SELECT 
    TotalSteps,
    VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes AS TotalActiveMinutes,
    Calories,
    TotalMinutesAsleep,
    TotalTimeInBed
  FROM merged_data
  WHERE TotalMinutesAsleep IS NOT NULL
")

# View the result
head(activity_sleep_analysis)

#Correlation Analysis
# Correlation between Total Steps and Sleep Metrics
cor_steps_sleep <- cor(activity_sleep_analysis$TotalSteps, activity_sleep_analysis$TotalMinutesAsleep, use = "complete.obs")
cor_steps_bed <- cor(activity_sleep_analysis$TotalSteps, activity_sleep_analysis$TotalTimeInBed, use = "complete.obs")

# Correlation between Active Minutes and Sleep Metrics
cor_active_sleep <- cor(activity_sleep_analysis$TotalActiveMinutes, activity_sleep_analysis$TotalMinutesAsleep, use = "complete.obs")
cor_active_bed <- cor(activity_sleep_analysis$TotalActiveMinutes, activity_sleep_analysis$TotalTimeInBed, use = "complete.obs")

# Correlation between Calories Burned and Sleep Metrics
cor_calories_sleep <- cor(activity_sleep_analysis$Calories, activity_sleep_analysis$TotalMinutesAsleep, use = "complete.obs")
cor_calories_bed <- cor(activity_sleep_analysis$Calories, activity_sleep_analysis$TotalTimeInBed, use = "complete.obs")

# Print correlations
print(paste("Correlation between Total Steps and Minutes Asleep:", cor_steps_sleep))
print(paste("Correlation between Total Steps and Time in Bed:", cor_steps_bed))
print(paste("Correlation between Active Minutes and Minutes Asleep:", cor_active_sleep))
print(paste("Correlation between Active Minutes and Time in Bed:", cor_active_bed))
print(paste("Correlation between Calories and Minutes Asleep:", cor_calories_sleep))
print(paste("Correlation between Calories and Time in Bed:", cor_calories_bed))

#PLOT FOR THE QUERIES

# Install and load ggplot2 if needed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Scatter plot: Total Steps vs Total Minutes Asleep
ggplot(activity_sleep_analysis, aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Total Steps vs Total Minutes Asleep",
       x = "Total Steps",
       y = "Total Minutes Asleep") +
  theme_minimal()

# Scatter plot: Calories vs Total Minutes Asleep
ggplot(activity_sleep_analysis, aes(x = Calories, y = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Calories vs Total Minutes Asleep",
       x = "Calories",
       y = "Total Minutes Asleep") +
  theme_minimal()

#Query 2
# Merge activity and sleep datasets if not already done
merged_data <- sqldf("
  SELECT a.*, s.TotalSleepRecords, s.TotalMinutesAsleep, s.TotalTimeInBed
  FROM activity a
  LEFT JOIN sleep s
  ON a.Id = s.Id AND a.ActivityDate = s.SleepDay
")

# Query for sedentary time vs sleep patterns
sedentary_sleep_analysis <- sqldf("
  SELECT 
    SedentaryMinutes,
    TotalMinutesAsleep,
    TotalTimeInBed
  FROM merged_data
  WHERE TotalMinutesAsleep IS NOT NULL
")

# View the result
head(sedentary_sleep_analysis)

# Correlation between Sedentary Minutes and Sleep Metrics
cor_sedentary_sleep <- cor(sedentary_sleep_analysis$SedentaryMinutes, sedentary_sleep_analysis$TotalMinutesAsleep, use = "complete.obs")
cor_sedentary_bed <- cor(sedentary_sleep_analysis$SedentaryMinutes, sedentary_sleep_analysis$TotalTimeInBed, use = "complete.obs")

# Print correlations
print(paste("Correlation between Sedentary Minutes and Total Minutes Asleep:", cor_sedentary_sleep))
print(paste("Correlation between Sedentary Minutes and Total Time in Bed:", cor_sedentary_bed))

# Install and load ggplot2 if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

#PLOT for QUERY

ggplot(sedentary_sleep_analysis, aes(x = SedentaryMinutes)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Density of Sedentary Minutes",
       x = "Sedentary Minutes",
       y = "Density") +
  theme_minimal()

#Another PLOT but for the Total Minutes

ggplot(sedentary_sleep_analysis, aes(x = TotalMinutesAsleep)) +
  geom_density(fill = "green", alpha = 0.4) +
  labs(title = "Density of Total Minutes Asleep",
       x = "Total Minutes Asleep",
       y = "Density") +
  theme_minimal()

#Query 3
# Load required package
if (!require(sqldf)) {
  install.packages("sqldf")
}
library(sqldf)

# Merge activity and sleep datasets
merged_data <- sqldf("
  SELECT a.*, s.TotalSleepRecords, s.TotalMinutesAsleep, s.TotalTimeInBed
  FROM activity a
  LEFT JOIN sleep s
  ON a.Id = s.Id AND a.ActivityDate = s.SleepDay
")
# Query to analyze activity intensity vs sleep
intensity_sleep_analysis <- sqldf("
  SELECT 
    VeryActiveMinutes,
    FairlyActiveMinutes,
    LightlyActiveMinutes,
    TotalMinutesAsleep
  FROM merged_data
  WHERE TotalMinutesAsleep IS NOT NULL
")

# View the first few rows
head(intensity_sleep_analysis)

# Correlation between Very Active Minutes and Sleep Duration
cor_veryactive_sleep <- cor(intensity_sleep_analysis$VeryActiveMinutes, intensity_sleep_analysis$TotalMinutesAsleep, use = "complete.obs")

# Print correlation
print(paste("Correlation between Very Active Minutes and Total Minutes Asleep:", cor_veryactive_sleep))

# Install and load ggplot2 if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

#PLOT for the Query

# Heatmap of Very Active Minutes vs Total Minutes Asleep
ggplot(intensity_sleep_analysis, aes(x = VeryActiveMinutes, y = TotalMinutesAsleep)) +
  stat_bin2d(bins = 30) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap of Very Active Minutes vs Total Minutes Asleep",
       x = "Very Active Minutes",
       y = "Total Minutes Asleep",
       fill = "Frequency") +
  theme_minimal()

#Query 4:
# Load sqldf if not already loaded
if (!require(sqldf)) {
  install.packages("sqldf")
}
library(sqldf)

# Merge activity and sleep datasets
merged_data <- sqldf("
  SELECT a.*, s.TotalSleepRecords, s.TotalMinutesAsleep, s.TotalTimeInBed
  FROM activity a
  LEFT JOIN sleep s
  ON a.Id = s.Id AND a.ActivityDate = s.SleepDay
")

# Query for calories burned vs sleep duration
calories_sleep_analysis <- sqldf("
  SELECT 
    Calories,
    TotalMinutesAsleep,
    TotalTimeInBed
  FROM merged_data
  WHERE TotalMinutesAsleep IS NOT NULL
")

# View the first few rows
head(calories_sleep_analysis)

# Correlation between Calories and Sleep Duration
cor_calories_sleep <- cor(calories_sleep_analysis$Calories, calories_sleep_analysis$TotalMinutesAsleep, use = "complete.obs")
cor_calories_bed <- cor(calories_sleep_analysis$Calories, calories_sleep_analysis$TotalTimeInBed, use = "complete.obs")

# Print correlations
print(paste("Correlation between Calories and Total Minutes Asleep:", cor_calories_sleep))
print(paste("Correlation between Calories and Total Time in Bed:", cor_calories_bed))

#PLOT for Query
# Heatmap for Calories vs Total Minutes Asleep
ggplot(calories_sleep_analysis, aes(x = Calories, y = TotalMinutesAsleep)) +
  stat_bin2d(bins = 30) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap: Calories Burned vs Total Minutes Asleep",
       x = "Calories Burned",
       y = "Total Minutes Asleep",
       fill = "Frequency") +
  theme_minimal()

#Query 5:

# Load sqldf if not already loaded
if (!require(sqldf)) {
  install.packages("sqldf")
}
library(sqldf)

# Merge activity and sleep datasets
merged_data <- sqldf("
  SELECT a.*, s.TotalSleepRecords, s.TotalMinutesAsleep, s.TotalTimeInBed
  FROM activity a
  LEFT JOIN sleep s
  ON a.Id = s.Id AND a.ActivityDate = s.SleepDay
")
# Install and load dplyr if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

# Calculate sleep consistency (standard deviation) and average activity metrics
sleep_consistency <- merged_data %>%
  group_by(Id) %>%
  summarise(
    SleepConsistency = sd(TotalMinutesAsleep, na.rm = TRUE),
    AvgTotalSteps = mean(TotalSteps, na.rm = TRUE),
    AvgCalories = mean(Calories, na.rm = TRUE),
    AvgVeryActiveMinutes = mean(VeryActiveMinutes, na.rm = TRUE)
  )

# View the results
head(sleep_consistency)

# Correlation between Sleep Consistency and Average Total Steps
cor_sleep_steps <- cor(sleep_consistency$SleepConsistency, sleep_consistency$AvgTotalSteps, use = "complete.obs")

# Correlation between Sleep Consistency and Average Calories Burned
cor_sleep_calories <- cor(sleep_consistency$SleepConsistency, sleep_consistency$AvgCalories, use = "complete.obs")

# Correlation between Sleep Consistency and Very Active Minutes
cor_sleep_active <- cor(sleep_consistency$SleepConsistency, sleep_consistency$AvgVeryActiveMinutes, use = "complete.obs")

# Print correlations
print(paste("Correlation between Sleep Consistency and Avg Total Steps:", cor_sleep_steps))
print(paste("Correlation between Sleep Consistency and Avg Calories:", cor_sleep_calories))
print(paste("Correlation between Sleep Consistency and Avg Very Active Minutes:", cor_sleep_active))

  #------------------------------------------------Activity & Weight --------------------------------------

#Query 1:

# Convert date formats in weight and activity datasets
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
weight$Date <- as.Date(weight$Date, format = "%m/%d/%Y")

# Merge weight and activity datasets
merged_data <- sqldf("
  SELECT a.*, w.WeightKg, w.BMI
  FROM activity a
  LEFT JOIN weight w
  ON a.Id = w.Id AND a.ActivityDate = w.Date
")

# Extract relevant columns for analysis
activity_weight_analysis <- sqldf("
  SELECT 
    WeightKg,
    BMI,
    TotalSteps,
    VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes AS TotalActiveMinutes
  FROM merged_data
  WHERE WeightKg IS NOT NULL
")

# View the result
head(activity_weight_analysis)

# Correlation between Weight and Activity Levels
cor_weight_steps <- cor(activity_weight_analysis$WeightKg, activity_weight_analysis$TotalSteps, use = "complete.obs")
cor_weight_active <- cor(activity_weight_analysis$WeightKg, activity_weight_analysis$TotalActiveMinutes, use = "complete.obs")

# Correlation between BMI and Activity Levels
cor_bmi_steps <- cor(activity_weight_analysis$BMI, activity_weight_analysis$TotalSteps, use = "complete.obs")
cor_bmi_active <- cor(activity_weight_analysis$BMI, activity_weight_analysis$TotalActiveMinutes, use = "complete.obs")

# Print correlations
print(paste("Correlation between Weight and Total Steps:", cor_weight_steps))
print(paste("Correlation between Weight and Total Active Minutes:", cor_weight_active))
print(paste("Correlation between BMI and Total Steps:", cor_bmi_steps))
print(paste("Correlation between BMI and Total Active Minutes:", cor_bmi_active))

# Install and load ggplot2 if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Scatter plot: Weight vs Total Steps
ggplot(activity_weight_analysis, aes(x = WeightKg, y = TotalSteps)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter Plot: Weight vs Total Steps",
       x = "Weight (Kg)",
       y = "Total Steps") +
  theme_minimal()

# Scatter plot: BMI vs Total Active Minutes
ggplot(activity_weight_analysis, aes(x = BMI, y = TotalActiveMinutes)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Scatter Plot: BMI vs Total Active Minutes",
       x = "BMI",
       y = "Total Active Minutes") +
  theme_minimal()

#Query 2:


# Convert dates to consistent formats
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
weight$Date <- as.Date(weight$Date, format = "%m/%d/%Y")

# Merge weight and activity datasets
merged_data <- sqldf("
  SELECT w.Date, w.Id, w.WeightKg, w.BMI, a.Calories, 
         a.VeryActiveMinutes, a.FairlyActiveMinutes, a.LightlyActiveMinutes
  FROM weight w
  LEFT JOIN activity a
  ON w.Id = a.Id AND w.Date = a.ActivityDate
")


# Calculate weight and BMI changes over time
weight_changes <- merged_data %>%
  group_by(Id) %>%
  arrange(Date) %>%
  mutate(
    WeightChange = WeightKg - lag(WeightKg),
    BMIChange = BMI - lag(BMI)
  )

# View the result
head(weight_changes)

#Query 3:
# Install and load sqldf if not already installed
if (!require(sqldf)) {
  install.packages("sqldf")
}
library(sqldf)

# Convert date formats to a consistent format
weight$Date <- as.Date(weight$Date, format = "%m/%d/%Y")
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

# Merge weight and sleep datasets
merged_data <- sqldf("
  SELECT w.Id, w.Date, w.WeightKg, w.BMI, s.TotalMinutesAsleep, s.TotalTimeInBed
  FROM weight w
  LEFT JOIN sleep s
  ON w.Id = s.Id AND w.Date = s.SleepDay
")

# Install and load dplyr if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

# Calculate average sleep duration and combine with weight and BMI
sleep_weight_analysis <- merged_data %>%
  group_by(Id) %>%
  summarise(
    AvgSleepDuration = mean(TotalMinutesAsleep, na.rm = TRUE),
    AvgWeight = mean(WeightKg, na.rm = TRUE),
    AvgBMI = mean(BMI, na.rm = TRUE)
  )

# View the result
head(sleep_weight_analysis)

# Install and load ggplot2 if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Scatter plot: Avg Sleep Duration vs Avg Weight
ggplot(sleep_weight_analysis, aes(x = AvgSleepDuration, y = AvgWeight)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter Plot: Avg Sleep Duration vs Avg Weight",
       x = "Average Sleep Duration (Minutes)",
       y = "Average Weight (Kg)") +
  theme_minimal()

# Scatter plot: Avg Sleep Duration vs Avg BMI
ggplot(sleep_weight_analysis, aes(x = AvgSleepDuration, y = AvgBMI)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Scatter Plot: Avg Sleep Duration vs Avg BMI",
       x = "Average Sleep Duration (Minutes)",
       y = "Average BMI") +
  theme_minimal()

#Query 4:
# Install and load sqldf if not already installed
if (!require(sqldf)) {
  install.packages("sqldf")
}
library(sqldf)

# Convert date formats to a consistent format
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
weight$Date <- as.Date(weight$Date, format = "%m/%d/%Y")

# Merge activity and weight datasets
merged_data <- sqldf("
  SELECT a.Id, a.ActivityDate, a.TotalSteps, a.Calories, w.WeightKg, w.BMI
  FROM activity a
  LEFT JOIN weight w
  ON a.Id = w.Id AND a.ActivityDate = w.Date
")

# Install and load dplyr if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

# Create categories and calculate calorie burn efficiency
calorie_efficiency <- merged_data %>%
  mutate(
    CaloriesPerStep = ifelse(TotalSteps > 0, Calories / TotalSteps, NA),  # Avoid division by zero
    WeightCategory = cut(WeightKg, breaks = c(0, 60, 80, 100, Inf), labels = c("Underweight", "Normal", "Overweight", "Obese")),
    BMICategory = cut(BMI, breaks = c(0, 18.5, 24.9, 29.9, Inf), labels = c("Underweight", "Normal", "Overweight", "Obese"))
  )

# View the result
head(calorie_efficiency)

# Average CaloriesPerStep by Weight Category
weight_summary <- calorie_efficiency %>%
  group_by(WeightCategory) %>%
  summarise(AvgCaloriesPerStep = mean(CaloriesPerStep, na.rm = TRUE))

# Average CaloriesPerStep by BMI Category
bmi_summary <- calorie_efficiency %>%
  group_by(BMICategory) %>%
  summarise(AvgCaloriesPerStep = mean(CaloriesPerStep, na.rm = TRUE))

# View summaries
weight_summary
bmi_summary

#PLOT for the QUERY

# Install and load ggplot2 if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Bar plot: Average CaloriesPerStep by Weight Category
ggplot(weight_summary, aes(x = WeightCategory, y = AvgCaloriesPerStep)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Average Calories Burned per Step by Weight Category",
       x = "Weight Category",
       y = "Average Calories Burned per Step") +
  theme_minimal()

# Bar plot: Average CaloriesPerStep by BMI Category
ggplot(bmi_summary, aes(x = BMICategory, y = AvgCaloriesPerStep)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Average Calories Burned per Step by BMI Category",
       x = "BMI Category",
       y = "Average Calories Burned per Step") +
  theme_minimal()


#Query 5:
# Install and load sqldf if not already installed
if (!require(sqldf)) {
  install.packages("sqldf")
}
library(sqldf)

# Convert dates to a consistent format
weight$Date <- as.Date(weight$Date, format = "%m/%d/%Y")
activity$ActivityDate <- as.Date(activity$ActivityDate, format = "%m/%d/%Y")
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

# Merge weight with activity and sleep datasets
merged_data <- sqldf("
  SELECT w.Id, w.Date, w.IsManualReport, w.WeightKg, w.BMI,
         a.TotalSteps, a.Calories, s.TotalMinutesAsleep, s.TotalTimeInBed
  FROM weight w
  LEFT JOIN activity a
  ON w.Id = a.Id AND w.Date = a.ActivityDate
  LEFT JOIN sleep s
  ON w.Id = s.Id AND w.Date = s.SleepDay
")

# Install and load dplyr if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

# Summarize activity and sleep metrics by IsManualReport
manual_vs_auto <- merged_data %>%
  group_by(IsManualReport) %>%
  summarise(
    AvgTotalSteps = mean(TotalSteps, na.rm = TRUE),
    AvgCalories = mean(Calories, na.rm = TRUE),
    AvgMinutesAsleep = mean(TotalMinutesAsleep, na.rm = TRUE),
    AvgTimeInBed = mean(TotalTimeInBed, na.rm = TRUE)
  )

# View the summary
manual_vs_auto

# T-test for Total Steps
t_steps <- t.test(TotalSteps ~ IsManualReport, data = merged_data, na.rm = TRUE)

# T-test for Calories
t_calories <- t.test(Calories ~ IsManualReport, data = merged_data, na.rm = TRUE)

# T-test for Minutes Asleep
t_sleep <- t.test(TotalMinutesAsleep ~ IsManualReport, data = merged_data, na.rm = TRUE)

# T-test for Time in Bed
t_bed <- t.test(TotalTimeInBed ~ IsManualReport, data = merged_data, na.rm = TRUE)

# Print t-test results
t_steps
t_calories
t_sleep
t_bed

# Install and load ggplot2 if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Boxplot: Total Steps by IsManualReport
ggplot(merged_data, aes(x = factor(IsManualReport), y = TotalSteps)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Total Steps: Manual vs Automated Logs",
       x = "Is Manual Report",
       y = "Total Steps") +
  theme_minimal()

# Boxplot: Calories by IsManualReport
ggplot(merged_data, aes(x = factor(IsManualReport), y = Calories)) +
  geom_boxplot(fill = "green", alpha = 0.7) +
  labs(title = "Calories: Manual vs Automated Logs",
       x = "Is Manual Report",
       y = "Calories") +
  theme_minimal()

# Boxplot: Minutes Asleep by IsManualReport
ggplot(merged_data, aes(x = factor(IsManualReport), y = TotalMinutesAsleep)) +
  geom_boxplot(fill = "purple", alpha = 0.7) +
  labs(title = "Minutes Asleep: Manual vs Automated Logs",
       x = "Is Manual Report",
       y = "Minutes Asleep") +
  theme_minimal()



