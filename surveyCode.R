#1 
library(tidyverse)
mh <- read_csv("survey.csv")

head(mh, 5)

#2 Filtering required columns and removing rows with null values
mh <- mh %>% select(Timestamp, Age, treatment) %>% drop_na()

#3 filtering data within 2014 and age between 18 and 70
mh <- mh %>% filter(substr(Timestamp, 1, 4) == "2014")
mh <- mh %>% filter(Age >= 18, Age <= 70)


