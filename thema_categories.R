library(tidyverse)
library(RODBC)

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials)

query <- "SELECT  [ThemaCategory_Key]
      ,[ThemaCode]
      ,[Level1_Danish]
      ,[Level1_English]
      ,[CustomerAura_ProductCategoryGroup]
      ,[CustomerAura_ProductCategory]
  FROM [EDW].[edw].[ThemaCategory]
where ThemaCategory_Key > 0"

df <- sqlQuery(channel,query)

df %>%
  map_df(n_distinct)

df %>%
  count(Level1_Danish) %>%
  arrange(desc(n))
