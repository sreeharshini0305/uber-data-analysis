library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) # metapackage of all tidyverse packages
library(DT)
library(scales)

#Creating vector of colors for the plots
colors = c("purple", "pink", "blue", "green", "yellow", "orange", "red")
colors

# Read the data for each month separately 
apr <- read.csv("F:/2-1/DV/archive/uber-raw-data-apr14.csv")
may <- read.csv("F:/2-1/DV/archive/uber-raw-data-may14.csv")
june <- read.csv("F:/2-1/DV/archive/uber-raw-data-jun14.csv")
july <- read.csv("F:/2-1/DV/archive/uber-raw-data-jul14.csv")
aug <- read.csv("F:/2-1/DV/archive/uber-raw-data-aug14.csv")
sept <- read.csv("F:/2-1/DV/archive/uber-raw-data-sep14.csv")

# Combine the data together 
data <- rbind(apr, may, june, july, aug, sept)
cat("The dimensions of the data are:", dim(data))

# Print the first 6 rows of the data
head(data)

data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)

# Create individual columns for month day and year
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))

# Add Time variables as well 
data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))


# Create a summary dataset 'hourly_data' from the 'data' dataset
hourly_data <- data %>%
  # Group the data by the 'hour' variable
  group_by(hour) %>%
  # Summarize the grouped data, counting the number of observations in each group
  dplyr::summarize(Total = n())
# Create a ggplot object with hourly_data, mapping hour to the x-axis and Total to the y-axis
ggplot(hourly_data, aes(hour, Total)) + 
  # Add bar geometry to represent the total number of trips for each hour
  geom_bar(stat="identity", fill="steelblue") +    
  # Set main title and subtitle for the plot
  # Adjust theme settings: remove legend, center main title and subtitle
  theme(legend.position = "none") +
  # Format y-axis labels with your desired format
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))


# Aggregate the data by month and hour to create a summary dataset 'month_hour_data'
month_hour_data <- data %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

# Create a bar plot using ggplot2
ggplot(month_hour_data, aes(hour, Total, fill = month)) + 
  
  # Add bar geometry, using 'month' for fill colors
  geom_bar(stat = "identity") + 
  
  # Set main title for the plot
  ggtitle("Trips by Hour and Month") + 
  
  # Format y-axis labels with commas for better readability
  scale_y_continuous(labels = comma)

# Aggregate data by day of the month to create a summary dataset 'day_data'
day_data <- data %>%
  group_by(day) %>%
  dplyr::summarize(Trips = n())

# Aggregate data by day of the week and month to create another summary dataset 'day_month_data'
day_month_data <- data %>%
  group_by(dayofweek, month) %>%
  dplyr::summarize(Trips = n())

# Plot the above data using ggplot2
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  
  # Add bar geometry with dodge positioning to distinguish months
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  
  # Set main title for the plot
  ggtitle("Trips by Day and Month") + 
  
  # Format y-axis labels with commas for better readability
  scale_y_continuous(labels = comma) + 
  
  # Manually set fill colors for each month
  scale_fill_manual(values = colors)

month_data <- data %>% 
  group_by(month) %>% 
  dplyr::summarize(Total = n())

ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "identity") +  # Corrected 'Identity' to 'identity'
  ggtitle("Trips in a Month") +  # Updated title for consistency
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# Aggregate data by day and hour to create a summary dataset 'day_hour_data'
day_hour_data <- data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())

# Plot a heatmap using ggplot2
ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
  geom_tile(color = "white") +  # Add tiles with white borders
  ggtitle("Heat Map by Hour and Day")

# Aggregate data by month and day to create a summary dataset 'month_day_data'
month_day_data <- data %>%
  group_by(month, day) %>%
  dplyr::summarize(Trips = n())

# Plot a heatmap using ggplot2
ggplot(month_day_data, aes(day, month, fill = Trips)) + 
  geom_tile(color = "white") +  # Add tiles with white borders
  ggtitle("Heat Map by Month and Day")
# Plot a heatmap by day of the week and month

ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")
