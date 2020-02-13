
library(tidyverse)
library(RODBC)
library(lubridate)

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(ggridges)



credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

#	  ,timeday.TimeOfDayHour
#,timeday.TimeOfDayMinute

query <- "SELECT Date_Key
  ,[Customer_Key]
      ,reader.[TimeOfDay_Key]
	  ,timeday.TimeOfDay


  FROM [EDW].[fact].[ReaderFact] as reader
  left join [EDW].[dim].[TimeOfDay] timeday on timeday.TimeOfDay_Key = reader.TimeOfDay_Key

  where Date_Key >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-45, 112)))"

df <- sqlQuery(channel,query)

write_csv(df,"example_data.csv")


df <- data.table::fread("example_data.csv") %>%
  distinct()

#df <- lazy_dt(df)

df <- df %>%
  filter(Customer_Key != -1) %>%
  mutate(Date = ymd(Date_Key),
         WeekDay = data.table::wday(Date),
         Time = parse_time(as.character(TimeOfDay))) %>%
  select(Date,Time,Customer_Key,WeekDay) %>%
  mutate(WeekDay = if_else(WeekDay > 1,WeekDay - 1,7)) # monday should be first day

keys <- df %>%
  sample_n(10000) %>%
  pull(Customer_Key)

keys = c(14796657,958983,1072962,1097690,12296292,14892116)
# example hour weekday
df %>%
  filter(Customer_Key %in% keys) %>%
  mutate(hour = lubridate::hour(Time)) %>%
  count(Customer_Key,WeekDay,hour) %>%
  as_tibble() %>%
  complete(Customer_Key,WeekDay=1:7,hour = 0:23,fill=list(n=0)) %>%
  ggplot(aes(factor(WeekDay),hour,fill=log(n+1))) +
  geom_tile() +
  scale_fill_gradient(low= "lightyellow",high = "darkred") +
  theme_minimal() +
  facet_wrap(~Customer_Key)

# density all users

df %>%
  as_tibble() %>%
  filter(Customer_Key %in% keys) %>%
  #sample_frac(.1) %>%
  ggplot(aes(x=Time,y=factor(Customer_Key),fill=stat(y))) +
  geom_density_ridges(alpha = .9,colour="snow") +
  theme_ridges() +
  scale_fill_viridis_c(option = "B")
  

# get mode
getmode <- function(v) {
uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}

# scatterplot 1 user, timing
df %>%
  filter(Customer_Key %in% keys) %>%
  group_by(WeekDay) %>%
  filter(!(abs(Time - median(Time)) > 2*sd(Time))) %>% # ignore outliers
  mutate(mT = min(Time)-5*60) %>%
  ungroup() %>%
  mutate(mD = getmode(WeekDay)) %>%
  ggplot(aes(x=Time,y=WeekDay)) +
  geom_jitter(alpha = .8) +
  geom_point(aes(mT,WeekDay,colour=WeekDay==mD),size=3)

tic()
d<-df %>%
  #sample_frac(.01) %>%
  filter(Customer_Key %in% keys) %>%
  group_by(Customer_Key) %>%
  mutate(mD = getmode(WeekDay)) %>%
  filter(abs(Time - median(Time)) < 2 * sd(Time),
         WeekDay == mD) %>% # ignore outliers
  group_by(Customer_Key,mD) %>%
  summarise(m = median(Time)) %>%
  as.data.table()
toc()

write_csv(d,"data_Day_Time.csv")
