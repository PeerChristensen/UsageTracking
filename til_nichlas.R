# Til Nichlas

# streamingandel pr. time og dag

library(tidyverse)
library(RODBC)
library(lubridate)


credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

#	  ,timeday.TimeOfDayHour
#,timeday.TimeOfDayMinute

query <- "SELECT Date_Key
	  ,timeday.TimeOfDay


  FROM [EDW].[fact].[ReaderFact] as reader
  left join [EDW].[dim].[TimeOfDay] timeday on timeday.TimeOfDay_Key = reader.TimeOfDay_Key

  where Date_Key >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-45, 112)))
and Customer_Key != -1"

df <- sqlQuery(channel,query)

df <- df %>%
  mutate(Date = ymd(Date_Key),
         Ugedag = data.table::wday(Date),
         Time = parse_time(as.character(TimeOfDay))) %>%
  select(Date,Time,Ugedag) %>%
  mutate(Ugedag = if_else(Ugedag > 1,Ugedag - 1,7)) 

# 11.312.938 streaming events

df <- df %>%
  mutate(Time = lubridate::hour(Time)) %>%
  count(Ugedag,Time) %>%
  as_tibble() %>%
  complete(Ugedag=1:7,Time = 0:23,fill=list(n=0)) %>%
  mutate(Ugedag = case_when(Ugedag == 1 ~ "Mandag",
                            Ugedag == 2 ~ "Tirsdag",
                            Ugedag == 3 ~ "Onsdag",
                            Ugedag == 4 ~ "Torsdag",
                            Ugedag == 5 ~ "Fredag",
                            Ugedag == 6 ~ "Lørdag",
                            Ugedag == 7 ~ "Søndag")) %>%
  mutate(Ugedag = fct_relevel(factor(Ugedag),"Mandag","Tirsdag","Onsdag","Torsdag","Fredag","Lørdag","Søndag"))

df <- df %>%
  group_by(Ugedag) %>%
  mutate(sum = sum(n)) %>%
  mutate(Andel = n/sum * 100) %>%
  select(-n,-sum) %>%
  ungroup()
  
df %>%
  ggplot(aes(Time,Andel,fill=Ugedag)) +
  geom_col() +
  facet_wrap(~Ugedag) +
  theme_minimal() +
  ggthemes::scale_fill_tableau() +
  theme(legend.position="none") +
  labs(y="Andel i %")

write_csv2(df, "streaming_andel_dag_time.csv")
