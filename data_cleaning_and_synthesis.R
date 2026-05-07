
# Attempt 2 --------------------------------------------------------------
library(tidyverse)
# First to create the population of each output area
import_1 <- read.csv("sapeoatablefinal2022v2.csv")
OA_population <- import_1 |> 
  select(1:4) 

names(OA_population) <- OA_population[3,]

while(OA_population$`LAD 2021 Name`[1] != "Hartlepool"){
  OA_population <- OA_population[-1,]
}

OA_population <- OA_population |> 
  rename("population" = "Total",
         "oa21cd" = "OA 2021 Code",
         "lad21cd" = "LAD 2021 Code",
         "lad21nm" = "LAD 2021 Name")

# Import and clean the RUC for the OA's 
import_2 <- read.csv("OA_RUC_classification.csv")

OA_RUC <- import_2 
names(OA_RUC) <- OA_RUC[2,]


while(OA_RUC$RUC21CD[1] != "UN1"){
  OA_RUC <- OA_RUC[-1,]
}
OA_RUC <- OA_RUC |> 
  janitor::clean_names()
# Joing together the OA population and OA RUC
OA_population_RUC <- full_join(OA_population, OA_RUC)

# Need to assign each of these OA's to their LSOA using a lookup directory 

import_3 <- read.csv("PCD_OA21_LSOA21_MSOA21_LAD_NOV24_UK_LU.csv", na.strings=c("","NA"))

OA_to_LSOA_mapping <- import_3 |> 
  distinct(oa21cd, .keep_all = TRUE) |> 
  select(oa21cd, lsoa21cd, lsoa21nm) |> 
  filter(!(is.na(lsoa21cd)))


# First identifying what OA codes are in the mapping but not the OA_population_RUC
intersect(OA_to_LSOA_mapping$oa21cd, OA_population_RUC$oa21cd)
# I think that these additional oacodes are due to the presence of NI in the mapping

# joining the OA_to_LSAO_mapping to the OA_population_RUC ~ by oa21cd
OA_population_RUC_mapped <- inner_join(OA_to_LSOA_mapping, OA_population_RUC, by = "oa21cd")

# Creating a new column for the population of urban areas in a OA
analysis_df1 <- OA_population_RUC_mapped |> 
  mutate(population = as.numeric(population), 
    urban_population = case_when(
    str_detect(rural_urban_flag, "Urban") == TRUE ~ population,
    str_detect(rural_urban_flag, "Urban") == FALSE ~ 0 
  )) |> 
  summarise(
    .by = lsoa21cd, 
    lsoa_population = sum(population),
    proportion_urban = sum(urban_population)/sum(population)
  )

# Importing the connectivity metrics for LSOA's
import_4 <- read.csv("connectivity_metrics_LSOA.csv")
connectivity_score <- import_4
names(connectivity_score) <- connectivity_score[2,]
while(connectivity_score$LSOA21CD[1] != "E01000001"){
  connectivity_score = connectivity_score[-1,]
}
connectivity_score <- connectivity_score |> 
  janitor::clean_names()

# Join together connectivity with the LSOA urban proportion 
connectivity_urban <- full_join(analysis_df1, connectivity_score)

# Adding in the further fron major town/city to the existing connectivity_urban

import_5 <- read.csv("supplementary_table_LSOA.csv")
relative_access <- import_5
names(relative_access) <- relative_access[2,]
while(relative_access$LSOA21CD[1] != "E01000001"){
  relative_access = relative_access[-1,]
}
relative_access <- relative_access |>
  janitor::clean_names() |> 
  select(-proportion_of_population_in_settlement_class_percent) |> 
  mutate(proportion_of_population_in_relative_access_category_percent = as.numeric(proportion_of_population_in_relative_access_category_percent),
    "proportion_closer_to_major_town" = case_when(
    str_detect(ruc21_relative_access, "Nearer to a major town or city") == TRUE ~ proportion_of_population_in_relative_access_category_percent,
    str_detect(ruc21_relative_access, "Further from a major town or city") == TRUE ~ 100 - proportion_of_population_in_relative_access_category_percent
  ), proportion_closer_to_major_town = as.numeric(proportion_closer_to_major_town)/100)

#collect this to the connectivity_urban to create the final csv
final_df <- full_join(relative_access, connectivity_urban)
write.csv(final_df, "collated_data_ready_for_analysis.csv")
