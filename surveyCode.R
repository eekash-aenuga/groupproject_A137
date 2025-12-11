#1 Importing data set and printing first five rows
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

#5 Create age groups
mh$AgeGroup <- cut(
  mh$Age,
  breaks = c(18, 25, 35, 45, 55, 65, 70),
  labels = c("18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 70"),
  right = TRUE,
  include.lowest = TRUE
)
age_treatment_table <- table(mh$AgeGroup, mh$treatment)
age_treatment_table

#6 Histogram and Curve
hist(mh$Age,
     breaks = 40,
     col = "lightblue",
     freq = FALSE,
     main = "Histogram of Age",
     xlab = "Age",
     xlim = c(15, 65)
)
axis(side = 1, at = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65))

curve(dnorm(x, mean = mean(mh$Age), sd = sd(mh$Age)),
      add = TRUE, lwd = 2)

#7 Calculate median age for each treatment group
aggregate(Age ~ treatment, median, data = mh)

#8 Mann–Whitney U Test
wilcox.test(Age ~ treatment, data = mh)

#9 Box Plot
boxplot(Age ~ treatment,
        data = mh,
        main = "Age Differences by Treatment Seeking",
        xlab = "Treatment (No / Yes)",
        ylab = "Age",
        col = c("lightyellow", "lightgreen"),
        ylim = c(15, 55)
)

axis(side = 2, at = seq(15, 55, by = 5))