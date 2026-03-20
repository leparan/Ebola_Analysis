# Ebola_Analysis
# Samwel Leparan
# 2025-05-12

#Load packages
pacman::p_load(
  tidyverse,
  inspectdf,
  plotly,
  janitor,
  visdat,
  esquisse
)

#Import and load the dataset
ebola_sierra_leone <- read_csv("data/ebola_sierra_leone.csv")

#Explore our data
head(ebola_sierra_leone)
tail(ebola_sierra_leone)

ncol(ebola_sierra_leone)
nrow(ebola_sierra_leone)
dim(ebola_sierra_leone)

summary(ebola_sierra_leone)
?dim

visdat::vis_dat(ebola_sierra_leone)

#categorical


#Numerical
num_summary<-inspect_num(ebola_sierra_leone)
show_plot(num_summary)

#Analysing single-variable
ebola_sierra_leone$age
mean(ebola_sierra_leone$age)

#descriptive stat
mean(ebola_sierra_leone$age,na.rm = TRUE)
median(ebola_sierra_leone$age,na.rm =T)
sd(ebola_sierra_leone$age,na.rm = T)
summary(ebola_sierra_leone$age)
length(ebola_sierra_leone$age)

#visualization for single var
age_vec<-ebola_sierra_leone$age
hist(age_vec)
boxplot(age_vec)


#ggplot
#esquisse
esquisser(ebola_sierra_leone)

ggplot(ebola_sierra_leone) +
  aes(x = age, y = sex) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()


#Analysing sinlge categorical var
ebola_sierra_leone$district
table(ebola_sierra_leone$district)
tabyl(ebola_sierra_leone$district)

tabyl(ebola_sierra_leone,district,sex)

barplot(table(ebola_sierra_leone$district))

#esquisser
esquisser(ebola_sierra_leone)

ggplot(ebola_sierra_leone) +
  aes(x = district, fill = district) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#when was he first case reported?
min(ebola_sierra_leone$date_of_sample)

#At the end of June 2014, which age-group had had most number of cases
tabyl(ebola_sierra_leone$status)
  
#what was the median Age of those affected?
median(ebola_sierra_leone$age,na.rm = T)

#Had there been most cases in men or women?
tabyl(ebola_sierra_leone$sex)
esquisser(ebola_sierra_leone)
ggplot(ebola_sierra_leone) +
  aes(x = sex, fill = sex) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
#which district had more reported cases?
tabyl(ebola_sierra_leone$district)
esquisser(ebola_sierra_leone)
ggplot(ebola_sierra_leone) +
  aes(x = district, fill = district) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#By the end of June 2024 was the ,was the outbreak growing receding?
esquisser(ebola_sierra_leone)
ggplot(ebola_sierra_leone) +
  aes(x = date_of_onset) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()

ebola_sierra_leone %>%
  filter(date_of_onset <= as.Date("2024-06-30")) %>%
  count(date_of_onset)