
# Get read time centers for all users
# Get cluster wss and scale
# Add best day and time time

# output:
# Customer Key
# Dag
# tid 1
# tid 1 wss score 1-5 (5 = stor spredning)
# tid 2, 3...

# kørselstid: 3t 20 min
# --------------------------------------------------------------------------------

library(tidyverse)
library(RODBC)
library(lubridate)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(ggridges)
library(factoextra)
library(ggthemes)
library(tictoc)
library(h2o)
library(hms)

tic()

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

# 	  ,timeday.TimeOfDayHour
#     ,timeday.TimeOfDayMinute

query <- "SELECT Date_Key
    ,[Customer_Key]
        ,reader.[TimeOfDay_Key]
  	  ,timeday.TimeOfDay
 
 
    FROM [EDW].[fact].[ReaderFact] as reader
    left join [EDW].[dim].[TimeOfDay] timeday on timeday.TimeOfDay_Key = reader.TimeOfDay_Key
 
    where Date_Key >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-45, 112)))
    and Customer_Key != -1"
 
df <- sqlQuery(channel,query)
 
 # write_csv(df,"example_data.csv")

# ----------------------------------------------------------------------------------------

#df <- data.table::fread("example_data.csv") %>%
#  distinct()
library(tictoc)
tic()
h2o.init(nthreads = 1)
h2o.no_progress()

set.seed(62)
keys <- df %>%
  distinct(Customer_Key) %>%
  sample_frac(.1) %>%
  pull(Customer_Key)

df <- df %>%
  filter(Customer_Key %in% keys)

df <- df %>%
  mutate(Date = ymd(Date_Key),s
         WeekDay = data.table::wday(Date),
         Time = parse_time(as.character(TimeOfDay))) %>%
  select(Customer_Key,Time,WeekDay) %>%
  mutate(WeekDay = if_else(WeekDay > 1,WeekDay - 1,7)) %>%
  mutate(WeekDay = case_when(WeekDay == 1 ~ "Monday",
                             WeekDay == 2 ~ "Tuesday",
                             WeekDay == 3 ~ "Wednesday",
                             WeekDay == 4 ~ "Thursday",
                             WeekDay == 5 ~ "Friday",
                             WeekDay == 6 ~ "Saturday",
                             WeekDay == 7 ~ "Sunday")) %>%
  mutate(secs = as.numeric(seconds(Time)))

# Get mode day
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

day_modes <- df %>%
  group_by(Customer_Key) %>%
  summarise(dayMode = getmode(WeekDay))
  
all_centers <- tibble()

for(key in keys){
  
  d <- df %>%
    filter(Customer_Key == key)
  
  if (n_distinct(d$Time) >= 20) {
  
    d <- as.h2o(d)
  
    km = h2o.kmeans(d,k = 3, x = c("Time"),estimate_k = T)
    
    centers <- km@model$centers %>% as_tibble()
    centers$time <- hms::hms(centers$time)
    
    centers$wss <- km@model$training_metrics@metrics$centroid_stats$within_cluster_sum_of_squares
   
    all_centers <- rbind(all_centers,cbind(Customer_Key = key,centers))
  }
  
}

all_keys <- enframe(df$Customer_Key) %>% select(Customer_Key = value) %>% distinct()

output <- all_centers %>% 
  mutate(wss = ntile(wss, 5),
         time = hms::round_hms(as_hms(time),secs=2)) %>%
  pivot_wider(names_from = centroid,values_from = c(time,wss)) %>% 
  left_join(day_modes) %>%
  select(Customer_Key, dayMode, time_1,wss_1,time_2,wss_2,time_3,wss_3) %>%
  full_join(all_keys)
  

toc()

#write_csv(output,"full_output.csv")

# df %>%
#   ggplot(aes(medianTime,sdTime,colour=factor(cluster))) +
#   geom_point(alpha = .5) +
#   scale_colour_tableau() +
#   theme_minimal()

# df <- read_csv("example_data_big.csv") %>%
#   mutate(Time = as.numeric(seconds(Time)))
