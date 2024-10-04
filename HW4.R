library(tidyverse)
library(sf)
library(ggthemes)
library(readr)
install.packages("lfe")
file_path = "/Users/likhitha/Documents/Data and Policy/uchicago-data-and-policy-scholar/HW4/HW4_Data"
setwd(file_path)
speed_tickets <- #speed_tickets is the dataframe for the violations
  read_csv("Speed_Camera_Violations.csv")
chetty <- read.csv("chetty_2000.csv")
us <- st_read("most_states/most_states.shp")


# Ques 1 ~ 2

# Then, write the code to generate a new dataframe that counts the total number of speed
# camera violations every year. Only consider from 2018 - 2021, so you should have four rows
# in the data: 2018, 2019, 2020, and 2021. How many violations took place in 2021?

violations_year <- speed_tickets %>%
  mutate(YEAR = year(mdy(`VIOLATION DATE`))) %>%
  filter(YEAR >= 2018 & YEAR <= 2021) %>%
  group_by(YEAR) %>%
  summarise(TOTAL_VIOLATIONS = sum(VIOLATIONS))



# Ques 3 ~ 4

# Now, create a dataframe which shows the monthly volume of speed camera violations in
# Children’s Safety Zones - Chicago. Again, focus your analysis only on observations pertaining
# to year 2018-2021. How many speed camera violations were in February, 2021, the month right
# before the policy took effect?

violations_month <- speed_tickets %>%
  mutate(DATE = mdy(`VIOLATION DATE`),
         YEAR = year(DATE),
         MONTH = month(DATE, label = TRUE, abbr = FALSE)) %>%
  filter(YEAR >= 2018 & YEAR <= 2021) %>%
  group_by(YEAR, MONTH) %>%
  summarise(TOTAL_VIOLATIONS = sum(VIOLATIONS, na.rm = TRUE)) %>%
  arrange(YEAR, MONTH)


# Ques 5 ~ 6

library(ggplot2)
library(scales)


violations_month$MONTH <- factor(violations_month$MONTH, levels = month.name)

ggplot(violations_month, aes(x = MONTH, y = TOTAL_VIOLATIONS / 1000, group = YEAR, color = factor(YEAR))) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  scale_x_discrete(name = "Month") +
  scale_y_continuous(name = "Total Camera Violations (in thousands)", 
                     labels = comma_format(scale = 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Monthly Speed Camera Violations in Children's Safety Zones (2018-2021)",
       color = "Year") +
  theme_clean() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )



# Ques 7 ~ 8

# 1. Begin with the speed_tickets dataset.
# 2. Only keep observations pertaining to the speed cameras which are first observed on or
# before January 1, 2018. In other words, if the earliest day that a particular CAMERA
# ID is observed in the dataset is after January 1, 2018, then all observations associated
# with that camera will be dropped from analysis
# 3. Next, only include the columns pertaining to CAMERA ID, violation date, and violations.
# Note that date should be converted to a lubridate format.
# 4. Reorganize the dataset in the following manner:
#   • Only include observations which take place in years 2018, 2019, 2020, and 2021.
# • Construct a new column entitled “post.” This is a binary treatment variable which
# is equal to 0 if the date is before March 1, 2021, and 1 if the date is on or after
# March 1, 2021.



speed_tickets <- speed_tickets %>%
  mutate(`VIOLATION DATE` = mdy(`VIOLATION DATE`)) %>%
  group_by(`CAMERA ID`) %>%
  filter(min(`VIOLATION DATE`, na.rm = TRUE) <= mdy("01/01/2018")) %>%
  ungroup() %>%
  
  select(`CAMERA ID`, `VIOLATION DATE`, VIOLATIONS) %>%
  
  # filtering
  filter(`VIOLATION DATE` >= mdy("01/01/2018"),
         `VIOLATION DATE` <= mdy("12/31/2021")) %>%
  mutate(post = as.integer(`VIOLATION DATE` >= mdy("12/31/2021")), month = month(`VIOLATION DATE`))
  
rowcount <- nrow(speed_tickets)


# Ques 9

library(lfe)

model <- felm(VIOLATIONS ~ post | factor(month) + factor(`CAMERA ID`) | 0 | `CAMERA ID`, data = speed_tickets)

summary(model)

# Ques 11

str(chetty)
tn_divorce <- chetty %>%
  filter(
    stateabbrv == "TN" & cs_divorced < 0.12
  ) %>%
  nrow()

library(maps)

state_divorce_rates <- chetty %>%
  group_by(stateabbrv) %>%
  summarize(avg_divorce_rate = mean(cs_divorced, na.rm = TRUE) * 100)

# Step 2: Join the state divorce rates with the map data
us_with_data <- us %>%
  left_join(state_divorce_rates, by = "stateabbrv")

us_with_data_filtered <- us_with_data %>%
  filter(stateabbrv != "AK")

ggplot(data = us_with_data_filtered) +
  geom_sf(aes(fill = factor(ntile(avg_divorce_rate, 5)))) +
  scale_fill_viridis_d(option = "plasma", name = "Divorce % Quintile",
                       labels = c("Lowest", "Low", "Medium", "High", "Highest")) +
  theme_minimal() +
  labs(title = "Quintile Map of Average Divorce Rate by State in the US") +
  theme(legend.position = "right") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE)

ggsave("divorce_percentage_map.png", width = 15, height = 15)






