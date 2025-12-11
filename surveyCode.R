#1 
library(tidyverse)
mh <- read_csv("survey.csv")

head(mh, 5)

#2 Filtering required columns and removing rows with null values
mh <- mh %>% select(Timestamp, Age, treatment) %>% drop_na()