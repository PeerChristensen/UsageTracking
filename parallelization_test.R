# parallelization test

library(h2o)
library(tidyverse)

h2o.init(nthreads = -1)
df <- read_csv("parallelisation_test.csv")

df <- df %>%
  select(Afternoon,EarlyMorning,Midday,Morning,Night) %>%
  mutate_all(scale)

hf <- as.h2o(df)

keys <- 1:nrow(df)


for (key in keys[1:10]) {
  
  km = h2o.kmeans(hf,k=3,estimate_k = T)
}
  
  
library(foreach)
library(doParallel)
library(h2o)
h2o.shutdown(prompt = FALSE)

#setup parallel backend to use 12 processors
cl <- makeCluster(8)
registerDoParallel(cl)

#loop
d <- foreach(i = keys[1:10], .combine=rbind) %dopar% {
  library(h2o)
  port <- 54321 + 3*i
  print(paste0("http://localhost:", port))
  h2o.init(nthreads = 1, max_mem_size = "1G", port = port)

  hf <- as.h2o(df)
  ss <- h2o.splitFrame(hf)
  km <- h2o.kmeans(ss[[1]],k=3,estimate_k = T)
  
  centers <- km@model$centers %>% as_tibble()
  d <- rbind(d,centers)
}
