library(tidyverse)
library(ggplot2)
library(dplyr)
file_path = "/Users/likhitha/Documents/Data and Policy/uchicago-data-and-policy-scholar/Capstone/ANES MATERIALS"
setwd(file_path)
anes_data <- read_csv("Data/ANES/anes_timeseries_cdf_csv_20220916.csv")
chetty_county <- read_csv("Data/Chetty/chetty_county.csv")


# filters for the range 1990-2000 and selects relevant variables
anes_data <- anes_data %>%
  filter(
    VCF0004 >= 1990 & VCF0004 <= 2000
  ) %>%
  select(VCF0901b, VCF0170d, VCF0004, VCF0114, VCF0148, VCF0148a, VCF0839)

names(anes_data)[names(anes_data) == "VCF0901b"] <- "stateabbrv"
names(anes_data)[names(anes_data) == "VCF0170d"] <- "cty2000"
names(anes_data)[names(anes_data) == "VCF0004"] <- "year"
names(anes_data)[names(anes_data) == "VCF0114"] <- "income_group"
names(anes_data)[names(anes_data) == "VCF0148"] <- "social_class_middle"
names(anes_data)[names(anes_data) == "VCF0148a"] <- "social_class_upper"
names(anes_data)[names(anes_data) == "VCF0839"] <- "govt_spending_scale"

chetty_county <- chetty_county %>%
  select(stateabbrv, cty2000, hhinc00, gini, inc_share_1perc, frac_middleclass, 
         cs00_seg_inc, cs00_seg_inc_pov25, cs00_seg_inc_aff75, taxrate, 
         subcty_total_taxes_pc, subcty_total_expenditure_pc, unemp_rate) 

# merge by county
merged_county <- left_join(anes_data, chetty_county, by = c("cty2000", "stateabbrv"))


merged_county <- merged_county %>%
  mutate(
    social_class = case_when(
      social_class_middle == 0 ~ "Lower",
      social_class_middle %in% c(1, 2, 3) | social_class_upper %in% c(1, 2, 3) ~ "Working",
      social_class_middle == 4 | social_class_upper == 4 ~ "Average Middle",
      social_class_middle == 5 | social_class_upper == 5 ~ "Middle",
      social_class_middle == 6 | social_class_upper == 6 ~ "Upper Middle",
      social_class_middle == 7 ~ "Upper",
      TRUE ~ NA_character_
    ),
    is_upper_class = ifelse(social_class %in% c("Upper Middle", "Upper"), 1, 0),
    income_percentile = case_when(
      income_group == 1 ~ "0-16",
      income_group == 2 ~ "17-33",
      income_group == 3 ~ "34-67",
      income_group == 4 ~ "68-95",
      income_group == 5 ~ "96-100",
      TRUE ~ NA_character_
    ),
    govt_spending_attitude = case_when(
      govt_spending_scale %in% c(1, 2) ~ "Spend Much Less",
      govt_spending_scale == 3 ~ "Spend Somewhat Less",
      govt_spending_scale == 4 ~ "Spend Same",
      govt_spending_scale == 5 ~ "Spend Somewhat More",
      govt_spending_scale %in% c(6, 7) ~ "Spend Much More",
      govt_spending_scale == 9 ~ "Don't Know",
      TRUE ~ NA_character_
    )
  )


merged_county <- merged_county %>%
  mutate(across(c(hhinc00, gini, inc_share_1perc, frac_middleclass), 
                ~scale(.) %>% as.vector(),
                .names = "{.col}_z"))



install.packages("lme4")
library(lme4)


merged_county$income_percentile <- factor(merged_county$income_percentile, 
                                          levels = c("0-16", "17-33", "34-67", "68-95", "96-100"))

merged_county$social_class <- factor(merged_county$social_class, 
                                     levels = c("Lower", "Working", "Average Middle", "Middle", "Upper Middle", "Upper"))




merged_county$govt_spending_attitude <- factor(merged_county$govt_spending_attitude, 
                                               levels = c("Spend Much Less", "Spend Somewhat Less", "Spend Same", 
                                                          "Spend Somewhat More", "Spend Much More", "Don't Know"))


install.packages("lfe")
library(lfe)

felm_model <- felm(govt_spending_scale ~ income_percentile + social_class + 
                     hhinc00_z + gini_z + inc_share_1perc_z + frac_middleclass_z | 
                     stateabbrv + year | 0 | stateabbrv, 
                   data = merged_county)

summary(felm_model)



library(dplyr)

ggplot(merged_county, aes(x = social_class, fill = govt_spending_attitude)) +
  geom_bar(position = "fill") +
  labs(title = "Government Spending Attitude by Social Class",
       x = "Social Class", y = "Proportion",
       fill = "Govt Spending Attitude") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

merged_county_clean <- merged_county %>%
  filter(!is.na(hhinc00) & !is.na(govt_spending_scale) & 
           is.finite(hhinc00) & is.finite(govt_spending_scale))

# relationship between county income and government spending attitude

median_incomes <- merged_county_clean %>%
  group_by(govt_spending_attitude) %>%
  summarise(median_income = median(hhinc00, na.rm = TRUE))

ggplot(merged_county_clean, aes(x = hhinc00)) +
  geom_density(adjust = 1.5, fill = "gray70", alpha = 0.7) +
  geom_vline(data = median_incomes, aes(xintercept = median_income), 
             linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~ govt_spending_attitude, scales = "free_y") +
  labs(title = "Distribution of County Income by Government Spending Attitude (1990-2000)",
       subtitle = "Red dashed line shows the median income",
       x = "County Household Income (Adjusted to 2000 Dollars)",
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar_format(), 
                     breaks = seq(20000, 80000, by = 20000)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "lightblue", color = "black"),
        strip.text = element_text(face = "bold"))



ggplot(merged_county, aes(x = income_percentile, fill = govt_spending_attitude)) +
  geom_bar(position = "fill") +
  labs(title = "Government Spending Attitude by Individual Income Percentile",
       x = "Income Percentile", y = "Proportion",
       fill = "Govt Spending Attitude") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(merged_county, aes(x = cut(frac_middleclass_z, breaks = 10), 
                          fill = factor(govt_spending_scale))) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(name = "Govt Spending Attitude",
                       labels = c("Much Less", "Less", "Somewhat Less", "Same", 
                                  "Somewhat More", "More", "Much More", "NA")) +
  labs(title = "Government Spending Attitude vs. Fraction of Middle Class",
       x = "Standardized Fraction of Middle Class (Binned)", 
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






top1_data <- merged_county %>%
  mutate(top1_quartile = case_when(
    is.na(inc_share_1perc_z) ~ "NA",
    TRUE ~ as.character(cut(inc_share_1perc_z, 
                            breaks = quantile(inc_share_1perc_z, probs = 0:4/4, na.rm = TRUE),
                            labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
                            include.lowest = TRUE))
  )) %>%
  mutate(top1_quartile = factor(top1_quartile, levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "NA")))


ggplot(top1_data, aes(x = top1_quartile, y = govt_spending_scale)) +
  geom_violin(aes(fill = top1_quartile)) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(4, "Purples"), "grey")) +
  labs(title = "Government Spending Attitude by Income Share of Top 1%",
       x = "Income Share of Top 1% Quartile", y = "Govt Spending Attitude Scale",
       fill = "Top 1% Share Quartile") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))







gini_data <- merged_county %>%
  mutate(gini_quartile = case_when(
    is.na(gini_z) ~ "NA",
    TRUE ~ as.character(cut(gini_z, 
                            breaks = quantile(gini_z, probs = 0:4/4, na.rm = TRUE), 
                            labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"), 
                            include.lowest = TRUE))
  )) %>%
  mutate(gini_quartile = factor(gini_quartile, levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "NA")))

ggplot(gini_data, aes(x = gini_quartile, y = govt_spending_scale)) +
  geom_boxplot(aes(fill = gini_quartile)) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = "Government Spending Attitude by County Gini Coefficient",
       x = "Gini Coefficient Quartile", y = "Govt Spending Attitude Scale",
       fill = "Gini Quartile") +
  theme_minimal() +
  theme(legend.position = "none")
















