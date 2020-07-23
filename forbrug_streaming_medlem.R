


library(RODBC)
library(tidyverse)

query1 <- "with 

cte1 as (SELECT [Customer_Key]
      ,[email]

  FROM [EDW].[edw].[Customer]

  where StreamingBenefit = 'Yes'),

cte2 as (SELECT
      [Customer_Key]
	  ,count(*) as n_bucketsYTD
  
  FROM [EDW].[edw].[NewReaderFact]
  
  where Date_Key >= 20200101
  group by Customer_Key),

cte3 as (select
	Customer_Key
	,sum(([MillisecondsSpent] / 1000) / 60) as totalMinutesYTD

	from EDW.edw.ReadDurationFact

	where ReadDurationDate_Key >= 20200101
	group by Customer_Key)

select email, cte2.n_bucketsYTD, cte3.totalMinutesYTD
from cte1
  inner join cte2 on cte2.Customer_Key = cte1.Customer_Key
  inner join cte3 on cte3.Customer_Key = cte1.Customer_Key
"

query2 <- "with 

cte1 as (SELECT [Customer_Key]
      ,[email]

  FROM [EDW].[edw].[Customer]

  where StreamingBenefit = 'Yes'),

cte2 as (SELECT
      [Customer_Key]
	  ,count(*) as n_bucketsApril
  
  FROM [EDW].[edw].[NewReaderFact]
  
  where Date_Key >= 20200401
  group by Customer_Key),

cte3 as (select
	Customer_Key
	,sum(([MillisecondsSpent] / 1000) / 60) as totalMinutesApril

	from EDW.edw.ReadDurationFact

	where ReadDurationDate_Key >= 20200401
	group by Customer_Key)

select email, cte2.n_bucketsApril, cte3.totalMinutesApril
from cte1
  inner join cte2 on cte2.Customer_Key = cte1.Customer_Key
  inner join cte3 on cte3.Customer_Key = cte1.Customer_Key
"

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

ytd <- sqlQuery(channel,query1) %>%
  as_tibble()
  
april <- sqlQuery(channel,query2) %>%
  as_tibble()

df <- inner_join(april,ytd) %>%
  arrange(desc(n_bucketsApril)) %>%
  top_n(1000)

write_csv2(df,"forbrug_april.csv")

library(emayili)
email <- envelope() %>%
  from("pech@saxo.com") %>%
  to("pech@saxo.com") %>% 
  subject("forbrug") %>%
  text("") %>% 
  attachment(glue::glue("forbrug_april.csv"))

creds <- read_rds("C:/Users/pech/Desktop/RProjects/ForecastAbo/email_credentials.rds")

smtp <- server(host     = creds[[1]],
               port     = creds[[2]],
               username = creds[[3]],
               password = creds[[4]],)

smtp(email, verbose = F)
