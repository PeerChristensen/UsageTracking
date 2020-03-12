# buckets

library(RODBC)
library(tidyverse)

query <- "
SELECT 
      [Customer_Key]
	  ,BucketNo
	  ,id,isbn13
	  ,title
 

  FROM [EDW].[fact].[ReaderFact] reader
  inner join [EDW].[dim].[ReadBucket] buck on buck.ReadBucket_Key = reader.ReadBucket_Key
  inner join [EDW].[dim].[Product] prod on prod.Product_Key = reader.Product_Key

  where Type = 'E-bøger'
		and Date_Key > 20200226
		and Customer_Key > 0
		and Premium_Key = 1
	

order by Customer_Key, BucketNo
"

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

buckets <- sqlQuery(channel,query) %>%
  as_tibble()


output <- buckets %>%
  select(Customer_Key,isbn13,title,BucketNo) %>%
  arrange(BucketNo) %>%
  mutate(checked = 1) %>%
  pivot_wider(id_cols = c(title,isbn13,Customer_Key), names_from = BucketNo,values_from = checked, 
              values_fill = list(checked = 0)) %>%
  unnest(`1`:`100`) %>%
  arrange(title)

view(output)  

buckets %>%
  select(Customer_Key,isbn13,title,BucketNo) %>%
  group_by(Customer_Key,title) %>%
  arrange(BucketNo) %>%
  mutate(gap= if_else(BucketNo != lag(BucketNo) + 1,"gap","1")) %>%
  pivot_wider(id_cols = c(title,isbn13,Customer_Key), names_from = BucketNo,values_from = gap, 
              values_fill = list(gap = NA)) %>%
  unnest(`1`:`100`) %>%
  arrange(title)



