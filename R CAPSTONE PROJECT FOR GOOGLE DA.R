### Cyclistic_Exercise_Full_Year_Analysis ###

# This analysis is for case study 1 from the Google Data Analytics Certificate (Cyclistic).  It’s originally based on the case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). We will be using the Divvy dataset for the case study. The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("/Users/jedga/Desktop/Capstone/Excel_files") #sets your working directory to simplify calls to data ... make sure to use your OWN username instead of mine ;)

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyselect)
library(lubridate)
library(ggplot2)
library(dplyr)
getwd()
setwd("/Users/PC/Documents/Rides Google/excel_files")

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
m1_2021 <- read.csv("202101-divvy-tripdata.csv")
m2_2021 <- read.csv("202102-divvy-tripdata.csv")
m3_2021 <- read.csv("202103-divvy-tripdata.csv")
m4_2020 <- read.csv("202004-divvy-tripdata.csv")
m5_2020 <- read.csv("202005-divvy-tripdata.csv")
m6_2020 <- read.csv("202006-divvy-tripdata.csv")
m7_2020 <- read.csv("202007-divvy-tripdata.csv")
m8_2020 <- read.csv("202008-divvy-tripdata.csv")
m9_2020 <- read.csv("202009-divvy-tripdata.csv")
m10_2020 <- read.csv("202010-divvy-tripdata.csv")
m11_2020 <- read.csv("202011-divvy-tripdata.csv")
M12_2020 <- read.csv("202012-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Comparing column names each of the files

colnames(m1_2021)
colnames(m2_2021)
colnames(m3_2021)
colnames(m4_2020)
colnames(m5_2020)
colnames(m6_2020)
colnames(m7_2020)
colnames(m8_2020)
colnames(m9_2020)
colnames(m10_2020)
colnames(m11_2020)
colnames(M12_2020)

# Rename columns  to make them consisent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
# Don't think this is necessary with the most recent data as of 4.17.2021

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type")
  
  # Inspecting the dataframes and look for inconguencies
  str(m1_2021)
  str(m2_2021)
  str(m3_2021)
  str(m4_2020)
  str(m5_2020)
  str(m6_2020)
  str(m7_2020)
  str(m8_2020)
  str(m9_2020)
  str(m10_2020)
  str(m11_2020)
  str(M12_2020) 
  
  # Converting ride_id and rideable_type to character so that they can stack correctly
  
  m1_2021 <-  mutate(m1_2021, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
  m2_2021 <-  mutate(m2_2021, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
  m3_2021 <-  mutate(m3_2021, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
  m4_2020 <-  mutate(m4_2020, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
  m5_2020 <-  mutate(m5_2020, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
  m6_2020 <-  mutate(m6_2020, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type))
  m7_2020 <-  mutate(m7_2020, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
  m8_2020 <-  mutate(m8_2020, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
  m9_2020 <-  mutate(m9_2020, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type))
  m10_2020 <-  mutate(m10_2020, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
  m11_2020 <-  mutate(m11_2020, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
  m12_2020 <-  mutate(M12_2020, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type))

  
  # Stack individual quarter's data frames into one big data frame
  all_trips <- bind_rows(m1_2021, m2_2021, m3_2021, m4_2020, m5_2020, m6_2020, m7_2020, m8_2020,
                         m9_2020, m10_2020, m11_2020, m12_2020)

  # Got an error since it looks like a couple columns are not matching. Running a different mutate script.
  # Here making all start_station_id = col_double(), and end_station_id = col_double(), some have it as character
  
  m1_2021 <-  mutate(m1_2021, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id)) 
  m2_2021 <-  mutate(m2_2021, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id)) 
  m3_2021 <-  mutate(m3_2021, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id)) 
  m4_2020 <-  mutate(m4_2020, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id))  
  m5_2020 <-  mutate(m5_2020, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id))  
  m6_2020 <-  mutate(m6_2020, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id)) 
  m7_2020 <-  mutate(m7_2020, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id)) 
  m8_2020 <-  mutate(m8_2020, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id)) 
  m9_2020 <-  mutate(m9_2020, start_station_id = as.double(start_station_id)
                     ,end_station_id = as.double(end_station_id)) 
  m10_2020 <-  mutate(m10_2020, start_station_id = as.double(start_station_id)
                      ,end_station_id = as.double(end_station_id))  
  m11_2020 <-  mutate(m11_2020, start_station_id = as.double(start_station_id)
                      ,end_station_id = as.double(end_station_id))  
  m12_2020 <-  mutate(M12_2020, start_station_id = as.double(start_station_id)
                      ,end_station_id = as.double(end_station_id)) 
  
  #kicked some errors out, trying the bind again
  
  all_trips <- bind_rows(m1_2021, m2_2021, m3_2021, m4_2020, m5_2020, m6_2020, m7_2020, m8_2020,
                         m9_2020, m10_2020, m11_2020, m12_2020)
  #looks good now!
  
  
  
  # Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020 (Think this is also obsolete)
  all_trips <- all_trips %>%  
    select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))
 
  #======================================================
  # STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
  #======================================================
  # Inspect the new table that has been created
  colnames(all_trips)  #List of column names
  nrow(all_trips)  #How many rows are in data frame?
  dim(all_trips)  #Dimensions of the data frame?
  head(all_trips)  #See the first 6 rows of data frame.  Also tail(qs_raw)
  str(all_trips)  #See list of columns and data types (numeric, character, etc)
  summary(all_trips)  #Statistical summary of data. Mainly for numerics 
  
  
  
  # Reassign to the desired values (going with the current 2020 labels)
  all_trips <-  all_trips %>% 
    mutate(member_casual = recode(member_casual
                                  ,"Subscriber" = "member"
                                  ,"Customer" = "casual"))
  
  # Checking to make sure the proper number of observations were reassigned
  table(all_trips$member_casual)
  
  # Adding columns that list the date, month, day, and year of each ride
  # This will allow aggregation ride data for each month, day, or year
  all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
  all_trips$month <- format(as.Date(all_trips$date), "%m")
  all_trips$day <- format(as.Date(all_trips$date), "%d")
  all_trips$year <- format(as.Date(all_trips$date), "%Y")
  all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
  
  # Adding a "ride_length" calculation to all_trips (in seconds)
 
  all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
  
  # Inspecting the structure of the columns
  str(all_trips)
  
  # Converting "ride_length" from Factor to numeric to run calculations on the data
  is.factor(all_trips$ride_length)
  all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
  is.numeric(all_trips$ride_length)
  
 
  all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
  
  
  #=====================================
  # STEP 4: CONDUCTING DESCRIPTIVE ANALYSIS
  #=====================================
  # Descriptive analysis on ride_length (all figures in seconds)
  mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
  median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
  max(all_trips_v2$ride_length) #longest ride
  min(all_trips_v2$ride_length) #shortest ride
  
  # Summarizing all four ride_length
  
  summary(all_trips_v2$ride_length)
  
  # Comparing members and casual users
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
  
  #There seems to be much longer rides for casual users
  
  # See the average ride time by each day for members vs casual users
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
  
  # Fixing the days of the week that are out of order. 
  all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # Running the average ride time by each day for members vs casual users
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
  
  # analyzing ridership data by type and weekday
  all_trips_v2 %>% 
    mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
    group_by(member_casual, weekday) %>%  #groups by usertype and weekday
    summarise(number_of_rides = n()							#calculates the number of rides and average duration 
              ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
    arrange(member_casual, weekday)								# sorts
  
  # Visualizing the number of rides by rider type
  all_trips_v2 %>% 
    mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual, weekday) %>% 
    summarise(number_of_rides = n()
              ,average_duration = mean(ride_length)) %>% 
    arrange(member_casual, weekday)  %>% 
    ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge")
  
  # Visualization for average duration
  all_trips_v2 %>% 
    mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual, weekday) %>% 
    summarise(number_of_rides = n()
              ,average_duration = mean(ride_length)) %>% 
    arrange(member_casual, weekday)  %>% 
    ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
    geom_col(position = "dodge")
  
 