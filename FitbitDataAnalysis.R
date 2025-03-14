# Load required libraries
library(tidyverse)
library(ggplot2)
library(janitor)
library(dplyr)
library(lubridate)
library(scales)

# Read the data
fitbit2 <- read.csv("E:/R-4.4.1/R assignment/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")

# Clean the data
fitbit2 <- drop_na(fitbit2)
fitbit2<-mutate(fitbit2, activeminutes = VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes )
# Format date column and add day of the week
fitbit2$ActivityDate <- mdy(fitbit2$ActivityDate)
fitbit2$dow <- weekdays(fitbit2$ActivityDate)

# Identify unique users
unique_users <- unique(fitbit2$Id)
num_unique_users <- length(unique_users)

# Summary statistics
fitbit_summary <- fitbit2 %>% 
  summarize(
    avg_steps = mean(TotalSteps), 
    avg_dist = mean(TotalDistance), 
    avg_cal = mean(Calories),
    max_steps = max(TotalSteps), 
    max_dist = max(TotalDistance), 
    max_cal = max(Calories)
  )

# Insight: Understanding these averages helps fitness companies create personalized goals for users.

# Reorder days of the week
fitbit2$dow <- factor(fitbit2$dow, 
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Total steps by day of the week
ggplot(fitbit2, aes(x = dow, y = TotalSteps)) +
  geom_bar(stat = "identity", fill = "maroon", alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Total Steps by Day of the Week", x = "Day of the Week", y = "Total Steps")

# Insight: Users tend to walk more on weekends, suggesting that fitness challenges or promotional campaigns should target weekends to maximize engagement.

# Activity minutes vs. weekday
ggplot(fitbit2, aes(x = dow, y = activeminutes)) +
  stat_summary(fun = mean, geom = "bar", fill = "brown", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Average Active Minutes per Day", x = "Day of the Week", y = "Active Minutes")

# Insight: Active minutes peak on certain days—fitness brands can use this insight to send reminders or suggest workouts on low-activity days.

# Calories burned by day of the week
ggplot(fitbit2, aes(x = dow, y = Calories)) +
  geom_bar(stat = "identity", fill = "orange", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Calories Burned by Day of the Week", x = "Day of the Week", y = "Calories Burned")

# Insight: Calories burned follows a similar pattern to active minutes, reinforcing that movement directly influences calorie expenditure.

# Activity pattern distribution
fitbit_long <- fitbit2 %>% 
  pivot_longer(cols = c(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes),
               names_to = "ActivityLevel",
               values_to = "Minutes")

ggplot(fitbit_long, aes(x = ActivityLevel, y = Minutes, fill = ActivityLevel)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Time Spent on Different Activity Levels", x = "Activity Level", y = "Minutes")

# Insight: Sedentary behavior is significant—potential opportunity for wearable brands to introduce inactivity alerts.

# Sedentary minutes by day of the week
ggplot(fitbit2, aes(x = dow, y = SedentaryMinutes, fill = dow)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 1.2) +
  theme_minimal() +
  labs(title = "Sedentary Minutes by Day of the Week", x = "Day of the Week", y = "Sedentary Minutes")

# Insight: High sedentary time on weekdays indicates potential for wellness programs targeting office workers.

# Import sleep data
sleep_data <- read.csv("E:/R-4.4.1/R assignment/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv")

# Clean and transform sleep data
sleep_data <- na.omit(sleep_data)
sleep_data$date <- parse_date_time(sleep_data$date, orders = "mdy HMS p")

# Extract date and time separately
sleep_data <- sleep_data %>% 
  mutate(
    date_only = as.Date(date),
    time_only = format(date, "%H:%M:%S"),
    day_of_week = wday(date, label = TRUE, abbr = FALSE),
    hour_only = hour(hms(time_only)) + minute(hms(time_only)) / 60
  )

# Total sleep hours per day of the week
sleep_summary <- sleep_data %>% 
  group_by(day_of_week) %>% 
  summarise(total_sleep_hours = mean(n()) / 60)

ggplot(sleep_summary, aes(x = day_of_week, y = total_sleep_hours, fill = day_of_week)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Total Sleep Hours Per Day of the Week", x = "Day of the Week", y = "Total Sleep Hours")

# Insight: Users tend to sleep more on weekends, indicating an opportunity for sleep improvement programs or targeted wellness content.

# Import heart rate data
heart_data <- read.csv("E:/R-4.4.1/R assignment/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv")

# Convert time column to DateTime format
heart_data$Time <- as.POSIXct(heart_data$Time, format= "%m/%d/%Y %I:%M:%S %p")

# Extract date and hour
heart_data <- heart_data %>% 
  mutate(date_only = as.Date(Time),
         hour_only = hour(Time))

# Average heart rate by hour
hourly_hr <- heart_data %>% 
  group_by(date_only, hour_only) %>% 
  summarise(avg_heart_rate = mean(Value, na.rm = TRUE), .groups = "drop")

ggplot(hourly_hr, aes(x = hour_only, y = avg_heart_rate, group = date_only, colour = date_only)) +
  geom_line(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Hourly Heart Rate Trends", x = "Hour of the Day", y = "Average Heart Rate")

# Insight: Heart rates are lowest in early hours (during sleep) and peak in the morning (likely due to workouts and daily activity). Wearable brands could use this insight for personalized recommendations.

# Join heart rate and activity data
heart_activity <- inner_join(heart_data, fitbit2, by = c("Id", "ActivityDate"))

# Insight: Combining heart rate with activity data can provide valuable insights on how physical activity affects cardiovascular health.
