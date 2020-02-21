
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



query <- "SELECT  [Date_Key]
,[TimeOfDay_Key]
,prod.[ThemaCategory_Key]
,[Level1_Danish]
,[Name_English]
,[Level1_English]
,[CustomerAura_ProductCategoryGroup]
,[CustomerAura_ProductCategory]

FROM [EDW].[fact].[ReaderFact] reader
left join [EDW].[edw].[ProductThemaCategoryFact] prod on prod.Product_Key = reader.Product_Key
left join [EDW].[edw].[ThemaCategory] theme on theme.ThemaCategory_Key = prod.ThemaCategory_Key

where Date_Key >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-7, 112)))"

df <- sqlQuery(channel,query)

df <- df %>% as_tibble()

