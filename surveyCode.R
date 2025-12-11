#1 
library(tidyverse)
mh <- read_csv("survey.csv")

head(mh, 5)

#2 Filtering required columns and removing rows with null values
mh <- mh %>% select(Timestamp, Age, treatment) %>% drop_na()

#3 filtering data within 2014 and age between 18 and 70
mh <- mh %>% filter(substr(Timestamp, 1, 4) == "2014")
mh <- mh %>% filter(Age >= 18, Age <= 70)


#4 validating the treatment column to 'Yes' or 'No'
mh <- mh %>%
  mutate(treatment = str_trim(treatment),
         treatment = case_when(
           str_to_lower(treatment) %in% c("yes","y","yes ") ~ "Yes",
           str_to_lower(treatment) %in% c("no","n","no ")  ~ "No",
           TRUE ~ as.character(treatment)))