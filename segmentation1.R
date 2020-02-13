
library(tidyverse)
library(RODBC)
library(lubridate)

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(ggridges)
library(factoextra)
library(ggthemes())
library(tictoc)

tic()
df <- read_csv("example_data_big.csv")

df <- df %>%
  group_by(Customer_Key) %>%
  summarize(medianTime = median(as.numeric(seconds(Time))),
            sdTime = sd(as.numeric(seconds(Time)))) %>%
  ungroup() %>%
  mutate(median_scaled = scale(medianTime),
         sd_scaled = scale(sdTime)) %>%
  drop_na()


fviz_nbclust(df[,4:5], kmeans, method = "wss")

km <- kmeans(df[,4:5],4)

df$cluster <- km$cluster

toc()

df %>%
  ggplot(aes(medianTime,sdTime,colour=factor(cluster))) +
  geom_point(alpha = .5) +
  scale_colour_tableau() +
  theme_minimal()
