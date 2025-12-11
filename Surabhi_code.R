#“Is there a relationship between employee age and whether they seek mental-health treatment?”


#2
mh <- mh %>% select(Timestamp, Age, treatment) %>% drop_na()

#3
mh <- mh %>% filter(substr(Timestamp, 1, 4) == "2014")
mh <- mh %>% filter(Age >= 18, Age <= 70)

#4 Standardise treatment values
mh <- mh %>%
  mutate(treatment = str_trim(treatment),
         treatment = case_when(
           str_to_lower(treatment) %in% c("yes","y","yes ") ~ "Yes",
           str_to_lower(treatment) %in% c("no","n","no ")  ~ "No",
           TRUE ~ as.character(treatment)
         ))

#5
hist(mh$Age,
     breaks = 40,
     col = "lightblue",
     freq = FALSE,
     main = "Histogram of Age",
     xlab = "Age",
     xlim = c(18, 60)
)
axis(side = 1, at = c(18, 20, 25, 30, 35, 40, 45, 50, 55, 60))


curve(dnorm(x, mean = mean(mh$Age), sd = sd(mh$Age)),
      add = TRUE, lwd = 2)

#6
mh$treat_num <- ifelse(mh$treatment == "Yes", 1, 0)
cor.test(mh$Age, mh$treat_num)

#7
t.test(Age ~ treatment, data = mh)

#8
boxplot(Age ~ treatment,
        data = mh,
        main = "Age Differences by Treatment Seeking",
        xlab = "Treatment (No / Yes)",
        ylab = "Age",
        col = c("lightyellow", "lightgreen"),
        ylim = c(15, 55)
)

axis(side = 2, at = seq(15, 55, by = 5))
