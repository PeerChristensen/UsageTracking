# segmentation 3

library(tidyverse)
library(lubridate)

df <- read_csv("streaming_segments.csv")

d <- sample_n(df,10000)

d <- d %>% mutate(h = cut_interval(secs, 24
                           ,labels = FALSE))

d$h <- d$h - 1


d %>%
  group_by(h) %>%
  summarise(
    m1 = min(secs),
    m2 = max(secs)) %>% 
  inner_join(d,by="h") %>%
  select(h,m1,m2) %>%
  distinct() %>%
  mutate(hour1 = lubridate::hour(seconds_to_period(m1)))%>%tail()
