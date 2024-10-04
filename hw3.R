library(tidyverse)
library(jsonlite)
folderpath <- "/Users/likhitha/Documents/Data and Policy/uchicago-data-and-policy-scholar/HW3"
setwd(folderpath)
crashes <- read_csv("Traffic_Crashes_-_Crashes.csv")

url <- "https://data.cityofchicago.org/resource/85ca-t3if.json?CRASH_MONTH=1&$limit=25000"

crashes_data <- fromJSON(url, simplifyVector = TRUE)

crashes_2023 <- crashes_data %>%
  mutate(crash_date = ymd_hms(crash_date)) %>%
  filter(year(crash_date) == 2023) 

freq_cause <- crashes_2023 %>%
  group_by(prim_contributory_cause) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Ques 3-4

highest_crashes_day <- crashes %>%
  group_by(CRASH_DAY_OF_WEEK) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1,2)


# Ques 5-6
crashes <- crashes %>%
  mutate(day_type = ifelse(CRASH_DAY_OF_WEEK %in% c(1, 7), 'Weekend', 'Weekday'))

crashes_on_weekends <- crashes %>%
  filter(day_type == "Weekend") %>%
  nrow()

print(crashes_on_weekends)

# Ques 7-8
crashes <- crashes %>%
  mutate(injuries_binary = (INJURIES_TOTAL >= 1))

lm <- lm(data = crashes, formula = injuries_binary ~ POSTED_SPEED_LIMIT)

summary(lm)


# Ques 9
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

# regression formula
lpm <-lm(data =crashes,formula =injuries_binary~POSTED_SPEED_LIMIT)
# Correction for heteroskedasticity where
coeftest(lpm,vcov =vcovHC(lpm,type ="HC1"))
  


