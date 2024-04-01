setwd("~/OneDrive - Harvard University/Resume/International Organizations/World Bank/McNamara/Rohini_Venkitaraman_Iyer_R")

if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("labelled", quietly = TRUE)) {
  install.packages("labelled")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("estimatr", quietly = TRUE)) {
  install.packages("estimatr")
}

library(haven)
library(dplyr)
library(labelled)
library(tidyr)
library(ggplot2)
library(estimatr)

GEM_Baseline <- read_dta("data/GEM_Baseline.dta")
GEM_Treatment_Status <- read_dta("data/GEM_Treatment_Status.dta")

############################# Task 1 - Data cleaning ##########################

# summary(GEM_Baseline)
# Commenting this function out so it doesn't print in the final output.
# But I used this function to get a sense of the data.

# Converting character variables that have only numbers into numeric
char <- c("HHID", "q122_father_attend_school", "w20_hours_unpaid_job99", 
          "s02_cash_savings")
GEM_Baseline[char] <- lapply(GEM_Baseline[char], as.numeric)
# HHID has 3 NAs, dropping these observations
GEM_Baseline <- GEM_Baseline[!is.na(GEM_Baseline$HHID), ]

# Detecting duplicates
GEM_Baseline$is_duplicate <- duplicated(GEM_Baseline) | 
  duplicated(GEM_Baseline, fromLast = TRUE)
GEM_Baseline$is_duplicate_HHID <- duplicated(GEM_Baseline$HHID) | 
  duplicated(GEM_Baseline$HHID, fromLast = TRUE)

table(GEM_Baseline$is_duplicate, useNA = "always")
table(GEM_Baseline$is_duplicate_HHID, useNA = "always")
# This indicates that the duplicates have the same data throughout and 
# so 1 of the entries of each pair can be dropped
GEM_Baseline <- unique(GEM_Baseline)

GEM_Baseline <- GEM_Baseline %>%
  select(-c("is_duplicate", "is_duplicate_HHID"))

table(GEM_Baseline$w02_paid_in_cash_job1, useNA = "always")

GEM_Baseline <- GEM_Baseline %>%
  mutate(w02_paid_in_cash_job1 = replace(w02_paid_in_cash_job1, 
                                         w02_paid_in_cash_job1 == "FOOD HAWKER", 
                                         "N33"),
         w02_paid_in_cash_job1 = replace(w02_paid_in_cash_job1, 
                                         w02_paid_in_cash_job1 == "HOT WAITERS", 
                                         "R23"),
         w02_paid_in_cash_job3 = replace(w02_paid_in_cash_job3, 
                                         w02_paid_in_cash_job3 == "..", "."))

# Merging baseline data with treatment status data
merged <- merge(GEM_Baseline, GEM_Treatment_Status, by="HHID", all.x=TRUE)
merged <- merged %>%
  set_variable_labels(HHID = "Household ID", treatment = "Treatment Status")

############################# Task 2A - Table #############################

# Converting savings from Kenyan Shillings to US Dollars
merged <- merged %>%
  mutate(cash_savings_usd = s02_cash_savings/135,
         jewellery_savings_usd = s04_jewellery_savings_value/135)

merged$total_savings_usd <- rowSums(merged[, c("cash_savings_usd", 
                                               "jewellery_savings_usd")], 
                                    na.rm = TRUE)

merged <- merged %>%
  mutate(total_savings_usd = ifelse(is.na(cash_savings_usd) & 
                                      is.na(jewellery_savings_usd), 
                                    NA, total_savings_usd))
merged <- merged %>%
  set_variable_labels(cash_savings_usd = "Cash savings (in USD)", 
                      jewellery_savings_usd = "Jewellery savings (in USD)", 
                      total_savings_usd = "Total savings (in USD)")

# Summary statistics for savings
get_var_label <- function(var_name, data) {
  var_label <- attr(data[[var_name]], "label")
  return(var_label)
}

summary_stats <- function(data, variables) {
  stats <- sapply(data[variables], function(x) {
    c(
      "Number of households" = sum(!is.na(x)),
      Mean = round(mean(x, na.rm = TRUE), 2),
      Median = round(median(x, na.rm = TRUE), 2),
      SD = round(sd(x, na.rm = TRUE), 2),
      Min = round(min(x, na.rm = TRUE), 2),
      Max = round(max(x, na.rm = TRUE), 2)
    )
  })
  return(stats)
}

summary_table <- summary_stats(merged, 
                               c("cash_savings_usd", "jewellery_savings_usd", 
                                 "total_savings_usd"))
summary_df <- as.data.frame(t(summary_table))
rownames(summary_df) <- sapply(c("cash_savings_usd", "jewellery_savings_usd", 
                                 "total_savings_usd"), 
                               get_var_label, data = merged)
print(summary_df)
# Number of households represents the NON-MISSING observations for each variable
write.csv(summary_df, "output/savings.csv")

# These descriptive statistics indicate the presence of 
# (1) negative values (which doesn't make sense wrt savings) and 
# (2) extreme values that impact the mean (which is highly different from the 
# median in all 3 cases).
# While considering total savings instead of cash savings or jewellery savings 
# individually helps us reduce the number of missing values in the savings variables,
# the wide range of the values for the total_savings variable could have 
# implications while performing regression analyis below.

############################# Task 2B - Graph #############################

# 1. Creating a job type level dataset

merged <- merged %>%
  rename(w02_paid_in_cash_job4 = w16_unpaid_job99)

keep <- c("HHID", "treatment", "w02_paid_in_cash_job1", "w02_paid_in_cash_job2", 
          "w02_paid_in_cash_job3", "w02_paid_in_cash_job4", "w07_hours_job1", 
          "w07_hours_job2", "w07_hours_job3", "w20_hours_unpaid_job99")
graph <- subset(merged, select=keep)

# Reshaping data
graph_long <- pivot_longer(graph, cols = starts_with("w02_paid_in_cash_job"), 
                           names_to = "job", values_to = "job_type")

graph_long <- graph_long %>%
  mutate(hours = case_when(
    job == "w02_paid_in_cash_job1" ~ w07_hours_job1,
    job == "w02_paid_in_cash_job2" ~ w07_hours_job2,
    job == "w02_paid_in_cash_job3" ~ w07_hours_job3,
    job == "w02_paid_in_cash_job4" ~ w20_hours_unpaid_job99,
    TRUE ~ NA_real_
  ))

graph_long <- graph_long %>%
  select(-c("w07_hours_job1", "w07_hours_job2", "w07_hours_job3", 
            "w20_hours_unpaid_job99"))

graph_long <- graph_long %>%
  mutate(jobn = case_when(
    job == "w02_paid_in_cash_job1" ~ "Paid job 1",
    job == "w02_paid_in_cash_job2" ~ "Paid job 2",
    job == "w02_paid_in_cash_job3" ~ "Paid job 3",
    job == "w02_paid_in_cash_job4" ~ "Unpaid work"))

graph_long <- graph_long %>%
  select(-"job")
graph_long <- graph_long %>%
  rename(job = jobn, work_code = job_type)

graph_long <- graph_long[, c("HHID", "job", "work_code", "hours", "treatment")]
haven::write_dta(graph_long, "output/GEM_job_type_R.dta")

# 2. Creating the graph

# Consolidating work codes
table(graph_long$work_code, useNA = "always")
graph_long <- graph_long %>%
  mutate(work_code = case_when(
    work_code == "" | work_code == "." ~ NA_character_,
    work_code == "No Paid Work" ~ "Reported as no paid work under paid jobs",
    substr(work_code, 1, 1) == "H" ~ "Household Services and Cleaning",
    substr(work_code, 1, 1) == "N" ~ "Nonformal and Other",
    substr(work_code, 1, 1) == "P" ~ "Professionals",
    substr(work_code, 1, 1) == "R" ~ "Retail, Food, Service",
    TRUE ~ "Unpaid work"
  ))
table(graph_long$work_code, useNA = "always")

graph_long$hours <- ifelse(graph_long$hours < 0, NA, graph_long$hours)

graph_long <- graph_long %>%
  mutate(treat_hours = case_when(treatment == 1 ~ hours), 
         control_hours = case_when(treatment == 0 ~ hours))
graph_long$treatment <- as.factor(graph_long$treatment)

treat <- graph_long %>%
  filter(treatment == 1) %>%
  group_by(job, work_code) %>%
  summarise(total_hours = sum(treat_hours, na.rm = TRUE))

control <- graph_long %>%
  filter(treatment == 0) %>%
  group_by(job, work_code) %>%
  summarise(total_hours = sum(control_hours, na.rm = TRUE))

graph1_treatment <- ggplot(treat, aes(x = job, y = total_hours, 
                                      fill = work_code)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1, 
                                          linetype = "dotted"), 
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        plot.title = element_text(face = "bold", margin = margin(t = 20, b = 10), 
                                  size = 14, hjust = 0.3), 
        legend.position = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  labs(title = "Total number of hours worked by job-type for TREATMENT group",
       x = "",
       y = "Total number of hours",
       fill = "") +  
  coord_cartesian(ylim = c(0, 20000))

print(graph1_treatment)
ggsave("output/graph1_treatment.png", plot = graph1_treatment, width = 8, 
       height = 6, dpi = 300)

graph2_control <- ggplot(control, aes(x = job, y = total_hours, 
                                      fill = work_code)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1, 
                                          linetype = "dotted"), 
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        plot.title = element_text(face = "bold", margin = margin(t = 20, b = 10), 
                                  size = 14, hjust = 0.3), 
        legend.position = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  labs(title = "Total number of hours worked by job-type for CONTROL group",
       subtitle = "Control group",
       x = "",
       y = "Total number of hours",
       fill = "") +  
  coord_cartesian(ylim = c(0, 20000))

print(graph2_control)
ggsave("output/graph2_control.png", plot = graph2_control, width = 8, 
       height = 6, dpi = 300)

# Note - Paid jobs 1, 2, and 3 report data of most recent jobs held since 
# beginning of 2012. However, unpaid work is reported for the past 7 days before 
# the date of the survey.
# From the graphs, women in the control group seem to engage in higher number of 
# “paid” labor hours as indicated in the difference in bar heights particularly 
# for jobs 2 and 3.
# Moreover, inferring from the shading based on work code, women in the control 
# group devote more hours to "retail, food, and service" and "professional" jobs
# as their 2nd and 3rd paid jobs respectively.
# The distribution within paid job 1 is similar for treatment and control groups, 
# except that women report more unpaid hours even within this category in the 
# control group.
# Unpaid hours in the last 7 days ranks highest and almost similar across both groups.

############################# Task 3 - Regression #############################

# Creating a variable that indicates total number of people living in a household
merged$hh_members <- rowSums(merged[, c("q130_a_husband", "q130_b_boyfriend", 
                                        "q130_c_father", "q130_d_mother", 
                                        "q130_e_stepfather", "q130_f_stepmother", 
                                        "q130_g_father_in_law", "q130_h_mother_in_law", 
                                        "q130_i_own_children", "q130_j_grandparents", 
                                        "q130_k_brothers", 
                                        "q130_l_sisters")], na.rm = TRUE)

# Creating a variable that indicates total hours of paid labor from all 3 paid jobs
merged$total_hours_paid <- rowSums(merged[, c("w07_hours_job1", "w07_hours_job2", 
                                              "w07_hours_job3")], 
                                   na.rm = TRUE)

reg1 <- lm_robust(total_savings_usd ~ q102_age + q105_attend_school + 
                    q120_a_vocational_training + q134_i_water_piped + 
                    q134_a_electricity + q134_c_television + 
                    q134_l_sewing_machine + w02_paid_in_cash + 
                    w20_hours_unpaid_job99 + m901_b_currently_married + 
                    m912_a_spouse_attend_school + m912_spouse_years_education + 
                    treatment + total_hours_paid + hh_members, data = merged, 
                  clusters = HHID, se_type = "stata")
summary(reg1)

summary_output <- capture.output(summary(reg1))
write.table(summary_output, "output/regression_summary.txt")

# The above regression uses total savings in USD as the outcome variable and factors 
# like age of the respondent, whether they attended school or received vocation 
# training (before this intervention), their household characteristics like having 
# piped water, and assets like sewing machine, electricity, television, and how 
# many household members (eating from the same pot), whether the women received 
# cash/kind payment for any work they did and the total no. of paid and 
# unpaid hours of labor they engaged in, their current marital status and their 
# spouses' education and their treatment status in the experiment as 
# independent variables to understand what affects household savings.

# Based on the above regression, the women's age, whether they went to school, 
# whether their spouse went to school, and whether they own a sewing machine are 
# factors that positively and significantly influence total savings. 
# The results are significant because p < 0.05 (except for age when it i < 0.1). 
# The regression is also clustered at the household level to account for within 
# household correlation. I have used se.type=stata to adjust for 
# heteroscedasticity and potential serial correlation in the errors.

# The fact that hours of paid labor, or household members, or hours of unpaid labor 
# are not significant predictors of household savings seemed counter-intuitive.
# Hence, I regress each variable individually below to check their impact of 
# household savings with standard errors clustered at the household level.

dep_var <- c("q102_age", "q105_attend_school", "q120_a_vocational_training", 
             "q131_residence", "q134_a_electricity", "q134_c_television", 
             "q134_i_water_piped", "q134_l_sewing_machine", "w02_paid_in_cash", 
             "w20_hours_unpaid_job99", "m901_b_currently_married", 
             "m912_a_spouse_attend_school", "treatment", "hh_members", 
             "total_hours_paid")

for (var in dep_var) {
  formula <- as.formula(paste("total_savings_usd ~ ", var))
  reg2 <- lm_robust(formula, data = merged, cluster = HHID)
  print(summary(reg2))
}

# None of the variables produce significant results here either. 
# My hypothesis is that missing data for key indicators like
# whether the respondent was paid in cash, whether they received vocational 
# training, etc. in underestimating the impact of these factors on total savings.
# Moreover, as noted in part 2A, the wide range of values in total_savings 
# (including negative values and outliers/extreme values) could also be 
# contributing to the insignificant results. i.e. the outliers in total_savings 
# could suppress the impact of other variables on total_savings.
# This kind of result and missing values issue are things I would discuss with my 
# supervisor to understand how researchers deal with them and navigate next steps.

# Further, other than the variables available in this dataset, 
# a few other indicators that I would be interested in observing as factors that 
# influece household savings from the broader survey would be -
# Migration indicators - like if they have ever lived outside Nairobi, 
# especially in an urban setting
# where they accumulate their savings
# whether they have a bank account
# age at start of marriage
# no. of children, etc.

################ Optional Task 4 - Randomization Evaluation ##################

# 1. I would create balance tables that compare basic sociodemographics like 
# age, education, marital status, no. of children, no. of household members, 
# household savings, assets, hours spent on paid v/s unpaid labor, etc. 
# for the treatment and the control groups to check if the randomization has 
# resulted in 2 groups that are similar on all other observable 
# and unobservable indicators prior to the beginning of the intervention. 
# This would help isolate and attribute any differences between the groups post 
# intervention to the treatment alone.

# I would create balance tables using t-tests to compare sample means of the two 
# groups on the factors listed in the previous point. 
# The expectation is that the t-test results for each variable would 
# NOT BE SIGNIFICANT indicating that treatment and control means 
# are not significantly different from each other for that variable. 
# Variables that I would use would be similar to the ones I used and 
# outlined in the regression section.

# A short example of using t-test for creating balance tables is coded below.

test_var <- c("q102_age", "q105_attend_school", "q120_a_vocational_training", 
              "q131_residence", "q134_l_sewing_machine", "w02_paid_in_cash", 
              "w20_hours_unpaid_job99", "m901_b_currently_married", 
              "m912_a_spouse_attend_school")

for (var in test_var) {
  formula <- as.formula(paste(var, "~ treatment"))
  t_test <- t.test(formula, data = merged)
  print(t_test)
}

# As expected, none of the p-values are significant which means that we can 
# assume that there are no significant differences between the treatment and 
# control groups wrt to these variables that have been tested for.
# But they could be significant for other variables so it is important to conduct
# these balance tests on as many comparison variables as possible and relevant 
# for the specific analysis

