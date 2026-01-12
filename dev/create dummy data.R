#set up dummy data for use in the 
library(tidyverse)

df <- read_csv("db_data.csv")
num.cols <- sapply(df, is.numeric)
df2 <- df

df2[num.cols] <- sapply(df2[num.cols], function(x){x * runif(length(x), min = 0.1, max = 0.9)})

write_csv(df2, "db_data_DUMMY.csv")
