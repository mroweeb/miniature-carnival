library(tidyverse)
 

# Attempting to use dummy variable linear regression 
# This was the failed attempt to use dummy variable regression to observe a relationship
rural.lm <- glm(overall ~ ruc21cd + poly(ruc21cd, 2) + poly(ruc21cd, 3), data = rural_transport_import, 
                family = "gaussian")
summary(rural.lm)


rural_transport_interval <- rural_transport_import |> 
  mutate(ruc21cd = parse_character(ruc21cd),
         rural_score = case_when(ruc21cd == "RSF1" ~ "1",
                                 ruc21cd == "RSN1" ~ "2",
                                 ruc21cd == "RLF1" ~ "3",
                                 ruc21cd == "RLN1" ~ "4",
                                 ruc21cd == "UF1" ~ "5", 
                                 ruc21cd == "UN1" ~ "6"),
    rural_score = as.numeric(rural_score))

rural_interval.lm <- glm(overall ~ rural_score, 
                         data = rural_transport_interval, 
                         family = "gaussian")
summary(rural_interval.lm)

# Using the new proportion urban variable
library(tidyverse)
library(gt)
df1 <- read.csv("collated_data_ready_for_analysis.csv")
lm1 <- glm(overall_public_transport ~ proportion_urban , data = df1, family = "gaussian")                
summary(lm1)
lm2 <- glm(overall_public_transport ~ proportion_closer_to_major_town, data = df1, family = "gaussian" )
summary(lm2)
lm3 <- glm(overall_public_transport ~ proportion_closer_to_major_town + proportion_urban, data = df1, family = "gaussian")
summary(lm3)
lm_experimental <- lm(overall_public_transport ~ ruc21_settlement_class, data = df1, family = "gaussian")
#creating table summarising the glm's 
table_1_tibble <- tibble(
  Predictor = c("Proportion of LSOA population living in an OA classified as urban", "Proportion of LSOA living near to a major town"),
  "t value" =c(201.5, 88.05),
  "Pr(>|t|)" = c("<2e-16", "<2e-16"), 
  "Gradient Estimate" = c(33.8190, 21.6777),
  "Standard Error" = c(0.1678, 0.2462)
)
table1 <- gt(table_1_tibble) |> 
  tab_header(
    title = "Table 1",
    subtitle = "Results from the regression analysis"
  )


# Testing the means of the data using the Welch Two Sample t-test

df2 <- df1 |> 
  summarise(
    .by = rural_urban_flag, 
    mean = mean(overall_public_transport), 
    sample_varience = var(overall_public_transport),
    n = n() 
  ) |> 
  mutate(standd = sqrt(sample_varience))

attach(df2)

significance <- (mean[1] - mean[2]) / sqrt((sample_varience[1]/n[1]) + (sample_varience[2]/n[2]))

#using the r t.test function to confirm calculations
group_1 <- df1 |> 
  filter(rural_urban_flag == "Urban") |> 
  select(overall_public_transport)
group_2 <- df1 |> 
  filter(rural_urban_flag == "Rural") |> 
  select(overall_public_transport)
ttest_1 <- t.test(group_1, group_2)

