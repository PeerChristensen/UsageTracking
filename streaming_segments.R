

library(tidyverse)
library(RODBC)

df <- read_csv("full_output.csv")

df <- df %>%
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
  #filter(wss_3 >=1) %>% 
  sample_n(30) %>%
  pull(Customer_Key)

df %>%
  filter(Customer_Key %in% keys) %>%
  pivot_longer(cols=starts_with("time")) %>%
  ggplot(aes(x=value,y=as.character(Customer_Key))) +
  geom_point() +
  geom_line()


query <- "SELECT 
      [Customer_Key]
	  ,Type
	  ,SaxoCategory_Level1Name
	  ,count(*) as n
     
  FROM [EDW].[fact].[ReaderFact] reader
  left join [EDW].[dim].[Product] prod on prod.Product_Key = reader.Product_Key
  left join  [EDW].[fact].[ProductSaxoCategoryFact] cat on cat.Product_Key = reader.Product_Key
  left join [EDW].[dim].[SaxoCategory] cat2 on cat2.SaxoCategory_Key = cat.SaxoCategory_Key
  where Date_Key >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-60, 112)))
  and TypeGroup = 'Dig'
  and Customer_Key != -1

  group by Customer_Key, Type, SaxoCategory_Level1Name

 "

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

df2 <- sqlQuery(channel,query) %>%
  as_tibble()

type <- df2 %>%
  group_by(Customer_Key,Type) %>%
  summarise(nType = sum(n)) %>%
  ungroup() %>%
  pivot_wider(id_cols = Customer_Key, names_from = Type, values_from = nType) %>%
  mutate(`E-bøger` = replace_na(`E-bøger`,0.1)) %>%
  mutate(Lydbøger = replace_na(Lydbøger,0.1)) %>%
  mutate(lyd_ratio = Lydbøger / `E-bøger`,
         format = if_else(Lydbøger >= `E-bøger`,"Lyd","E-bog"))
  
category <- df2 %>%
  group_by(Customer_Key,SaxoCategory_Level1Name) %>%
  summarise(cType = sum(n)) %>%
  ungroup() %>%
  pivot_wider(id_cols = Customer_Key, names_from = SaxoCategory_Level1Name, values_from = cType) %>%
  select(- `NA`) %>%
  mutate(Fagbøger = replace_na(Fagbøger,0.1)) %>%
  mutate(Skønlitteratur = replace_na(Skønlitteratur,0.1)) %>%
  mutate(fiction_ratio = Skønlitteratur / Fagbøger,
         litt_pref = if_else(Skønlitteratur >= Fagbøger,"Skøn","Fag"))

output <- df %>%
  inner_join(type) %>%
  inner_join(category) %>%
  select(Customer_Key, time_type, Weekday, format, litt_pref) %>%
  filter(time_type != "None")

wss = NULL
for (i in 1:15) {
  km <- klaR::kmodes(output[,-1],modes = i)
  wss[i] = mean(km$withindiff)
}
  
plot(1:15,wss,type="b",pch=19) # 3 is best

km <- klaR::kmodes(output[,c(-1,-7)],modes = 3)

output$cluster <- km$cluster

output <- output %>%
  mutate(Segment = case_when(
    cluster == 1 ~ "Allday - Workday - Lyd - Skøn",
    cluster == 2 ~ "Other - Workday - E-bog - Skøn",
    cluster == 3 ~ "Allday - Weekend - Lyd - Skøn"
  ))

output %>%
  group_by(time_type, Weekday, format, litt_pref) %>%
  count() %>%
  mutate(Segment = paste0(time_type," - ", Weekday," - ", format," - ", litt_pref)) %>%
  ggplot(aes(reorder(Segment,n),n)) +
  geom_col() +
  coord_flip() +
  theme_minimal()
  

