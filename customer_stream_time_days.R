# For each subscriber, get

# median streaming time (hour:min:sec)
# most frequent hour (mode)
# most frequent day (mode)
# weekday vs weekend (ratio)

library(tidyverse)
library(RODBC)
library(lubridate)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tictoc)

tic()

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

query <- "SELECT Date_Key
          ,[Customer_Key]
         -- ,reader.[TimeOfDay_Key]
	        ,timeday.TimeOfDay


  FROM [EDW].[fact].[ReaderFact] as reader
  left join [EDW].[dim].[TimeOfDay] timeday on timeday.TimeOfDay_Key = reader.TimeOfDay_Key

  where Date_Key >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-45, 112)))
  and Customer_Key != -1"

df <- sqlQuery(channel,query)

# write_csv(df,"example_data.csv")
# 
# df <- vroom::vroom("example_data.csv") %>%
#   distinct()

#df <- lazy_dt(df)
#keys = c(14796657,958983,1072962,1097690,12296292,14892116,1139872)

df <- df %>%
 # filter(Customer_Key %in% keys) %>%
  mutate(Date = ymd(Date_Key), 
         WeekDay = data.table::wday(Date),
         Time = parse_time(as.character(TimeOfDay))) %>%
  select(Date,Time,Customer_Key,WeekDay) %>%
  mutate(WeekDay = if_else(WeekDay > 1,WeekDay - 1,7)) %>% # monday should be first day
  mutate(WeekDayName = case_when(WeekDay == 1 ~ "Mon",
                                 WeekDay == 2 ~ "Tue",
                                 WeekDay == 3 ~ "Wed",
                                 WeekDay == 4 ~ "Thu",
                                 WeekDay == 5 ~ "Fri",
                                 WeekDay == 6 ~ "Sat",
                                 WeekDay == 7 ~ "Sun")) %>%
  mutate(weekend = ifelse(WeekDay >=6,"yes","no"))
  
df <- lazy_dt(df)

# get most frequent weekday
weekdays <- df %>%
  count(Customer_Key,WeekDayName) %>%
  group_by(Customer_Key) %>%
  top_n(1,n) %>%
  select(-n)

# get most frequent hour
hours <- df %>%
  mutate(Hour = lubridate::hour(Time)) %>%
  count(Customer_Key,Hour) %>%
  group_by(Customer_Key) %>%
  top_n(1,n) %>%
  select(-n)

# get most frequent weekday combined with hour
day_hour_combined <- df %>%
  mutate(Hour = lubridate::hour(Time)) %>%
  count(Customer_Key,WeekDayName,Hour) %>%
  group_by(Customer_Key) %>%
  top_n(1,n) %>%
  select(-n) %>%
  rename(DayCombined =WeekDayName, HourCombined = Hour)
 

# get weekend ratio
weekend <- df %>%
  count(Customer_Key,weekend) %>%
  pivot_wider(names_from = weekend,values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(sum = no + yes) %>%
  mutate(weekendRatio = yes / sum) %>%
  select(Customer_Key,weekendRatio)
  
# get median time
median_time <- df %>%
  group_by(Customer_Key) %>%
 # filter(abs(Time - median(Time)) < 3 * sd(Time)) %>%
  group_by(Customer_Key) %>%
  summarise(MedianTime = median(Time)) 


data <- list(weekdays, hours, day_hour_combined, weekend, median_time)

output <- data %>% 
  reduce(full_join, by ="Customer_Key") %>% 
  distinct(Customer_Key,.keep_all = T)

output

toc()


