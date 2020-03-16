
library(tidyverse)
# library(RODBC)
# 
# query <- "select
# 
# *
# --sum(CAST(MillisecondsSpent AS BIGINT)) as msec
# 
# FROM EDW.edw.ReadDurationFact
# 
# order by ReadDurationDate_Key" 
# 
# 
# credentials <- read_rds("credentials.rds")
# 
# channel <- odbcConnect(credentials[1],credentials[2],credentials[3])
# 
# df <- sqlQuery(channel,query) %>%
#   as_tibble() %>%
#   mutate(Date = lubridate::ymd(ReadDurationDate_Key)) %>%
#   select(Date,Customer_Key,Product_Key,msec = MillisecondsSpent) %>%
#   mutate(msec = as.numeric(msec)) %>%
#   filter( Date < lubridate::today()-3,
#           Customer_Key > 0)


df <- read_csv("clean_duration_data.csv")

# avg tid per kunde per dag
tid <- df %>%
  #filter(Customer_Key == 12010588) %>%
  group_by(Date) %>%
  summarise(time = sum(msec),
            users = n_distinct(Customer_Key)) %>%
  mutate(avg_time_msec = time / users) %>%
  mutate(avg_time_secs = avg_time_msec / 1000) %>%
  mutate(avg_time_mins = avg_time_secs / 60) 

tid %>%
  ggplot(aes(Date,avg_time_mins)) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,150))

# median og avg tid per kunde per dag
df %>%
  group_by(Date,Customer_Key) %>%
  summarise(t = sum(msec)/ 1000 / 60) %>%
  group_by(Date) %>%
  summarise(median = median(t),
            mean = mean(t))
  

# avg antal titler per kunde per dag
n_titler <- df %>% 
  group_by(Date,Customer_Key) %>% 
  count() %>% group_by(Date) %>% 
  summarise(m=mean(n))

n_titler %>%
  ggplot(aes(Date,m)) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,2))

