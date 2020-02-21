
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



query <- "SELECT [Date_Key]
,prod.Product_Key
,[TimeOfDay]
,prod.[ThemaCategory_Key]
,[Level1_Danish]
,[Name_English]
,[Level1_English]
,[CustomerAura_ProductCategoryGroup]
,[CustomerAura_ProductCategory]

FROM [EDW].[fact].[ReaderFact] reader
left join [EDW].[edw].[ProductThemaCategoryFact] prod on prod.Product_Key = reader.Product_Key
left join [EDW].[edw].[ThemaCategory] theme on theme.ThemaCategory_Key = prod.ThemaCategory_Key
left join [EDW].[dim].[TimeOfDay] time on time.TimeOfDay_Key = reader.TimeOfDay_Key

where Date_Key >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-7, 112)))"

df <- sqlQuery(channel,query) %>%
  as_tibble()

# mystisk !
df %>% filter(Product_Key == 68869269) %>% group_by(TimeOfDay) %>% count()



df %>% group_by(Level1_Danish) %>% count() %>% arrange(-n)
df %>% group_by(Name_English) %>% count() %>% arrange(-n)
df %>% group_by(CustomerAura_ProductCategoryGroup) %>% count() %>% arrange(-n)
df %>% group_by(CustomerAura_ProductCategory) %>% count() %>% arrange(-n)

#  CustomerAura_ProductCategoryGroup heatmaps
df1 <- df %>%
  mutate(Date = ymd(Date_Key),
         WeekDay = data.table::wday(Date),
         Time = parse_time(as.character(TimeOfDay))) %>%
  select(Date,Time,CustomerAura_ProductCategoryGroup,WeekDay) %>%
  mutate(WeekDay = if_else(WeekDay > 1,WeekDay - 1,7)) # monday should be first day


df1 %>%
  drop_na() %>%
  mutate(hour = lubridate::hour(Time)) %>%
  count(CustomerAura_ProductCategoryGroup,WeekDay,hour) %>%
  as_tibble() %>%
  complete(CustomerAura_ProductCategoryGroup,WeekDay=1:7,hour = 0:23,fill=list(n=0)) %>%
  ggplot(aes(factor(WeekDay),hour,fill=n)) +
  geom_tile() +
  scale_fill_gradient(low= "lightyellow",high = "darkred") +
  theme_minimal() +
  facet_wrap(~CustomerAura_ProductCategoryGroup)

# individually..
fict <- df1 %>%
  filter(CustomerAura_ProductCategoryGroup == "Skønlitteratur") %>%
  mutate(hour = lubridate::hour(Time)) %>%
  count(CustomerAura_ProductCategoryGroup,WeekDay,hour) %>%
  as_tibble() %>%
  #complete(CustomerAura_ProductCategoryGroup,WeekDay=1:7,hour = 0:23,fill=list(n=0)) %>%
  ggplot(aes(factor(WeekDay),hour,fill=n)) +
  geom_tile() +
  scale_fill_gradient(low= "lightyellow",high = "darkred") +
  theme_minimal() +
  ggtitle("Skønlitteratur") +
  theme(legend.position = "none")


fact <- df1 %>%
  filter(CustomerAura_ProductCategoryGroup == "Faglitteratur") %>%
  mutate(hour = lubridate::hour(Time)) %>%
  count(CustomerAura_ProductCategoryGroup,WeekDay,hour) %>%
  as_tibble() %>%
  #complete(CustomerAura_ProductCategoryGroup,WeekDay=1:7,hour = 0:23,fill=list(n=0)) %>%
  ggplot(aes(factor(WeekDay),hour,fill=n)) +
  geom_tile() +
  scale_fill_gradient(low= "lightyellow",high = "darkred") +
  theme_minimal() +
  ggtitle("Faglitteratur") +
  theme(legend.position = "none")

gridExtra::grid.arrange(fict,fact,ncol=2)


# danish level1
df2 <- df %>%
  mutate(Date = ymd(Date_Key),
         WeekDay = data.table::wday(Date),
         Time = parse_time(as.character(TimeOfDay))) %>%
  select(Date,Time,Level1_Danish,WeekDay) %>%
  mutate(WeekDay = if_else(WeekDay > 1,WeekDay - 1,7)) # monday should be first day


df2 %>%
  drop_na() %>%
  mutate(Level1_Danish = factor(str_remove(as.character(Level1_Danish),"Bøger\\|"))) %>%
  ggplot(aes(x=Time,y=Level1_Danish,fill=stat(y))) +
  geom_density_ridges(alpha = .9,colour="snow") +
  theme_ridges() +
  scale_fill_viridis_c(option = "B")


# til clustering

# df <- df %>%
#   mutate(Date = ymd(Date_Key),
#          WeekDay = data.table::wday(Date),
#          Time = parse_time(as.character(TimeOfDay))) %>%
#   select(CustomerAura_ProductCategoryGroup,Time,WeekDay) %>%
#   mutate(WeekDay = if_else(WeekDay > 1,WeekDay - 1,7)) %>%
#   mutate(WeekDay = case_when(WeekDay == 1 ~ "Monday",
#                              WeekDay == 2 ~ "Tuesday",
#                              WeekDay == 3 ~ "Wednesday",
#                              WeekDay == 4 ~ "Thursday",
#                              WeekDay == 5 ~ "Friday",
#                              WeekDay == 6 ~ "Saturday",
#                              WeekDay == 7 ~ "Sunday")) %>%
#   mutate(Time = as.numeric(seconds(Time)))



