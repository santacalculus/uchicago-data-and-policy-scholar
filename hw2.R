# libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)

folder_path <- "/Users/likhitha/Documents/Data and Policy/HW2/Datasets"
setwd(folder_path)
getwd()
list.files(folder_path)
people <- read_csv("Traffic_Crashes_-_People.csv")
exists("people")
crashes <- read_csv("Traffic_Crashes_-_Crashes.csv")
exists("crashes")
fert <- read_csv("wave_3_fertility.csv")
exists("fert")
power <- read_csv("wave_3_power.csv")
exists("power")


# Questions

# Question 1 ~ 2
# 
# Please follow the instructions in the HW description to download and load the dataset:
#   “Traﬀic_Crashes_-_People.csv”. Spend some time exploring the names of the different
# columns of the datasets, and how they are organized.
# Among female sex individuals of age 65 and above in this dataset, what proportion are pedestrians?

# condition, sex = females
# condition, age >= 65
# condition, person type = pedestrian

people %>%
  filter(
    SEX == "F",
    AGE >= 65,
  ) %>%
  summarise(
    total = n(),
    total_p = sum(PERSON_TYPE == "PEDESTRIAN"),
    mean = total_p / total
  )

#   Hint: Look at columns entitled AGE, SEX, and PERSON_TYPE. After selecting in the
# dataset only observations with female sex and age equal to or above 65, calculate what for
# what proportion PERSON_TYPE is PEDESTRIAN



# Question 3 ~ 4
# Again using the dataset entitled “Traﬀic_Crashes_-_People.csv”, answer the following.
# Filter the dataset to only those with no indication of injury. How old is the oldest person in
# this filtered dataset? Use the INJURY_CLASSIFICATION and AGE columns to answer this
# question.

people %>%
  filter(
    INJURY_CLASSIFICATION == "NO INDICATION OF INJURY"
  ) %>%
  summarise(
    oldest_person = max(AGE, na.rm = TRUE)
  )


# Question 5 ~ 6
# Again using the dataset entitled “Traﬀic_Crashes_-_People.csv”, answer the following.
# How many people in the dataset are age 25 or younger and are passengers? Use PER-
#   SON_TYPE and AGE to answer this question

# condition, age <= 25
# condition, person_type passenger

people %>%
  filter(
    AGE <= 25,
    PERSON_TYPE == "PASSENGER"
  ) %>%
  summarise(
    pass_25 = n()
  )


# Question 7 ~ 8
# Begin with the “Traﬀic_Crashes_-_People.csv” dataset.
# • Filter for a subset of observations pertaining to individuals who are age 65 or older.
# • Next, Join with the crashes dataset.
# • Get the year from CRASH_DATE
# How many distinct people age 65 or above were involved in a car crash during the year 2020?

# condition, age >= 65
# condition, CRASH_DATE = 2020

library(lubridate)

people %>%
  filter(
    AGE >= 65
  ) %>%
  inner_join(
    crashes,
    by = "CRASH_RECORD_ID"
  ) %>%
  mutate(CRASH_YEAR = year(mdy_hms(CRASH_DATE))) %>%
  filter(CRASH_YEAR == 2020) %>%
  distinct(PERSON_ID) %>%
  nrow()

# people %>%
#   filter(AGE >= 65) %>%
#   left_join(crashes, by="CRASH_RECORD_ID") %>%
#   mutate(crash_date_lubridate = mdy_hms(CRASH_DATE)) %>%
#   mutate(crash_year = year(crash_date_lubridate)) %>%
#   filter(crash_year == 2020) %>%
#   distinct(PERSON_ID) %>%
#   nrow()


# Question 9

# Your task is to plot the number of people who died in car crashes over time (every year)
# and submit an image of your final result in a lineplot, as well as your code in the following
# question.
# • Begin with the people dataset, and keep only the observations where the variable IN-
#   JURY_CLASSIFICATION indicates a fatal injury.
# • Next, join with the crashes dataset and get the year associated with each person who
# got into fatal car accident.
# • Use ggplot to make a lineplot which illustrates the number of people who died in car
# crashes every year.

fatal_injuries <- people %>% 
  filter(
    INJURY_CLASSIFICATION == "FATAL"
  ) %>%
  left_join(crashes, by = "CRASH_RECORD_ID") %>%
  mutate(CRASH_YEAR = year(mdy_hms(CRASH_DATE))) 

deaths_per_year <- fatal_injuries %>%
  group_by(CRASH_YEAR) %>%
  summarize(deaths = n())
  
ggplot(deaths_per_year, aes(x=CRASH_YEAR, y=deaths)) +
  geom_line() +
  geom_point() +
  labs(
    title="Number of People Who Died in Car Crashes Every Year",
    x = "Year",
    y = "Number of Deaths"
  ) +
  scale_x_continuous(breaks = seq(min(fatalities_per_year$CRASH_YEAR), 
                                  max(fatalities_per_year$CRASH_YEAR), 
                                  by = 1))

ggsave("fatal_crashes_over_time.png", width = 10, height = 6)


# Question 10 ~ 11
# Note: For this question, please follow the instructions in the HW description to download
# and load the dataset: “wave_3_fertility.csv”. Spend some time exploring the names of the
# different columns of the datasets, and how they are organized. For many of the questions
# below, make sure to refer to the codebook linked in page 1.
# Among women age 40 and above who have given birth to a child in this dataset, what propor-
#   tion have never given birth to a girl?
#   Hint: Look at columns entitled age, evergivenbirth, and borngirls. After selecting in the
# dataset only observations who have given birth and age equal to or above 40, calculate what
# for what proportion borngirls is zero.

# condition, age >= 40
# condition, evergivenbirth = 1

fert %>%
  filter(
    age >= 40,
    evergivenbirth == 1
  ) %>%
  summarise(
    born_girls_prop = mean(borngirls == 0)
  )



# Question 12 ~ 13
# Again using the dataset entitled “wave_3_fertility.csv”, answer the following.
# How many women in the dataset are age 20 or younger and have been pregnant in the past 12
# months? Use pregnantlastyear and age to answer this question.

# condition, age <= 20
# condition, pregnantlastyear

fert %>% 
  filter(
    age <= 20,
    pregnantlastyear == 1
  ) %>%
  summarise(
    women_20_lastyear = n()
  )

# Question 14 ~ 15
# Begin with the “wave_3_fertility.csv” dataset.
# • Filter to observations of women that have children (borntotal >= 0).
# • Next, join with the “wave_3_power.csv” dataset along two columns, FPrimary and
# hhmid. These correspond to household and household member respectively.
# • Count the number of rows
# How many respondents with only daughter(s) believe it is better to send a son to school than
# a daughter (bettersontoschool = 1)? What if the respondent has only son(s)?

fert %>% 
  filter(
    borntotal >= 0
  ) %>%
  left_join(power, by = c("FPrimary", "hhmid")) %>%
  # nrow()
  filter(
    (borngirls > 0 & bornboys == 0 & bettersontoschool == 1) | (borngirls == 0 & bornboys > 0 & bettersontoschool == 1)
  ) %>%
  summarise(
    daughters = sum(borngirls > 0 & bornboys == 0),
    sons = sum(bornboys > 0 & borngirls == 0)
  ) 

# Your task is to plot a histogram of the number of children born to women who can no longer
# have children and submit an image of your final result, as well as your code in the following
# question.
# • Begin with the fertility dataset and keep only the observations where the variable age-
#   menopause is not equal to -1.
# • Use ggplot to make a scatterplot which shows number of children born to women who
# are different ages. You must properly label your graph.
# • You must include a title and labels on the x and y axes and pick a fill color of your choice
# for the bars

women_who_can_not_have_children <- fert %>%
  filter(
    agemenopause != -1
  ) %>%
  ggplot(aes(x = borntotal)) +
  geom_histogram(fill="pink") +
  labs(
    title="The Number of Children Born to Women Who Can Longer Have Children",
    x = "Number of Women",
    y = "Number of Children"
  )
print(women_who_can_not_have_children)






  
  



