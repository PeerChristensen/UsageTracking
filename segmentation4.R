# segmentation 3

library(tidyverse)
library(lubridate)
library(h2o)

h2o.init()

df <- read_csv("streaming_segments.csv")

keys <- df %>%
  count(Customer_Key) %>%
  filter(n>=20) %>%
  #sample_frac(.3) %>% 
  pull(Customer_Key)

df <- df %>%
  filter(Customer_Key %in% keys)

df <- df %>%
  mutate(hour = lubridate::hour(seconds_to_period(secs))) %>%
  filter(!WeekDay %in% c("Saturday","Sunday"))

df <- df %>%
  mutate(timeOfDay = case_when(hour >= 22 | hour <= 2 ~ "Night",
                               hour >= 3 & hour <= 6 ~ "EarlyMorning",
                               hour >= 7 & hour <= 10 ~ "Morning",
                               hour >= 11 & hour <= 14 ~ "Midday",
                               hour >= 15 & hour <= 18 ~ "Afternoon",
                               hour >= 19 & hour <= 21 ~ "Evening")) %>%
  select(Customer_Key,timeOfDay) %>%
  group_by(Customer_Key,timeOfDay) %>%
  count() %>%
  group_by(Customer_Key) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(prop_n = n / sum_n) %>%
  ungroup() %>%
  select(Customer_Key, timeOfDay, prop_n) %>%
  arrange(timeOfDay) %>%
  pivot_wider(names_from = timeOfDay, values_from = prop_n)

df[is.na(df)] <- 0

Customer_Key <- df$Customer_Key

hf <- df %>% 
  select(-Customer_Key) %>% 
  scale() %>%
  as.h2o()

# elbow plot
d <- NULL
for (i in 1:25) {
  
  km = h2o.kmeans(hf,k=i)
  
  wss = tibble(centroids = i, 
               wss =km@model$model_summary$within_cluster_sum_of_squares)
  
  d = rbind(d,wss)
}

d %>%
  ggplot(aes(centroids,wss)) +
  geom_line() +
  geom_point()

km1 = h2o.kmeans(hf,k = 3)

centers1 <- km1@model$centers %>% as_tibble()

km2 = h2o.kmeans(hf,k = 6)

centers2 <- km2@model$centers %>% as_tibble()

centers2 %>%
  pivot_longer(-centroid) %>%
  mutate(name = fct_relevel(factor(name),"earlymorning","morning","midday","afternoon",
                            "evening","night")) %>%
  mutate(
         centroid = as.numeric(centroid)) %>%
  group_by(centroid) %>%
  arrange(name) %>%
  mutate(row = row_number()) %>%
  ggplot(aes(row,value),group=centroid) +
  geom_line() +
  facet_wrap(~centroid) +
  scale_x_continuous(labels = c("earlymorning","morning","midday","afternoon",
                                "evening","night"),breaks=1:6) +
  theme(axis.text.x = element_text(angle = 45))



segments1 <- predict(km1,hf) %>% as_tibble()
df$segment1 <- segments1$predict + 1

segments2 <- predict(km2,hf) %>% as_tibble()
df$segment2 <- segments2$predict + 1

df2 <- read_csv("full_output.csv")

df2 <- df2 %>%
  mutate(Weekday = if_else(dayMode == "Sunday" | dayMode == "Saturday","Weekend","Workday"),
         time_type = case_when(
           is.na(wss_2) & wss_1 >= 4             ~ "Allday",
           time_1 > lubridate::hms("05:30:00") & 
             time_1 < lubridate::hms("10:00:00") &
             time_2 > lubridate::hms("14:00:00") &
             time_2 < lubridate::hms("18:30:00") &
             Weekday == "Workday"                  ~ "Commuter",
           # time_1 < lubridate::hms("04:00:00")   & 
           #   time_2 > lubridate::hms("21:00:00") & 
           #   is.na(time_3)                         ~ "Nighttime",
           time_1 < lubridate::hms("04:00:00")   & 
             (time_2 > lubridate::hms("21:00:00") | time_3 > lubridate::hms("21:00:00")) ~ "Nighttime",
           !is.na(time_3) == T ~"multiple",
           is.na(time_1)                           ~ "None",
           TRUE                                    ~ "Other"))

keys <- df %>%
df2 %>%
  inner_join(df) %>%
  select(-contains("wss"),-dayMode,-Customer_Key,-time_3) %>%
  select(time_1,time_2,EarlyMorning,Morning,Midday,Afternoon,Evening,Night,segment1,segment2)

# df <- df %>% 
#   mutate(hour = cut_interval(secs, 24
#                            ,labels = FALSE))
# 
# if (min(df$hour) == 1) {
#   df$hour <- df$hour - 1
# }
# 
# df %>%
#   group_by(hour) %>%
#   summarise(
#     m1 = min(secs),
#     m2 = max(secs)) %>% 
#   inner_join(df,by="hour") %>%
#   select(hour,m1,m2) %>%
#   distinct() %>%
#   mutate(hour1 = lubridate::hour(seconds_to_period(m1)))%>%tail()