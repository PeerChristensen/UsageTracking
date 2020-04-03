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
  select(Customer_Key,hour) %>%
  group_by(Customer_Key,hour) %>%
  count() %>%
  group_by(Customer_Key) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(prop_n = n / sum_n) %>%
  ungroup() %>%
  select(Customer_Key, hour, prop_n) %>%
  arrange(hour) %>%
  pivot_wider(names_from = hour, values_from = prop_n)
  
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

km = h2o.kmeans(hf,estimate_k = T)

centers <- km@model$centers %>% as_tibble()

centers %>%
  pivot_longer(-centroid) %>%
  mutate(name = as.numeric(name),
         centroid = as.numeric(centroid)) %>%
  ggplot(aes(name,value)) +
  geom_line() +
  facet_wrap(~centroid)

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