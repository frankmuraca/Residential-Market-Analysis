## CoStar data ===========================
## =======================================

library(tidyverse)
library(here)

# Read in CoStar data ======================
## 


costar_all_trend <- read_csv(here("data","costar_all_props_trends.csv")) %>%
  clean_names()


costar_all_trend_original_cols <- colnames(costar_all_trend)

colnames(costar_all_trend) <- paste0("all_props_",costar_all_trend_original_cols)


rm(costar_all_trend_original_cols)


costar_ab_trend <- read_csv(here("data","costar_ab_props_trends.csv")) %>% clean_names()
costar_ab_trend_original_cols <- colnames(costar_ab_trend)
colnames(costar_ab_trend) <- paste0("ab_props_",costar_ab_trend_original_cols)


rm(costar_ab_trend_original_cols)

costar_c_trend <- read_csv(here("data","costar_c_props_trends.csv")) %>% clean_names()
costar_c_trend_original_cols <- colnames(costar_c_trend)
colnames(costar_c_trend) <- paste0("c_props_",costar_c_trend_original_cols)


costar_merge_trend <- cbind(costar_all_trend,costar_ab_trend,
                            costar_c_trend)



## Current conditions of ab properties ================================

current_conditions <- costar_ab_trend %>% 
  filter(ab_props_period == "2022 YTD") %>%
  select(ab_props_inventory_units,
         ab_props_vacancy_percent,
         ab_props_effective_rent_per_unit) %>%
  mutate(ab_props_vacancy_percent = as.double(str_remove(ab_props_vacancy_percent,"%"))) %>%
  mutate(ab_props_effective_rent_per_unit = as.double(str_remove(ab_props_effective_rent_per_unit,"\\$"))) %>%
  pivot_longer(cols = c(ab_props_inventory_units,
                        ab_props_vacancy_percent,
                        ab_props_effective_rent_per_unit)) %>%
  mutate(name = recode(name,
                       ab_props_inventory_units = "Units",
                       ab_props_vacancy_percent = "CoStar Vacancy rate (%)",
                       ab_props_effective_rent_per_unit = "Effective Rent per unit"
  )) %>%
  mutate(value = as.character(value)) 


## Clean database of individual properties ===========================

costar_all_props <- read_csv(here("data","costar_all_props.csv"))

costar_all_props_clean <- costar_all_props %>% 
  select(`Property Address`,`Property Name`,City,`Building Class`,`Year Built`, `Number Of Units`, RBA,
         `Avg Effective/Unit`,`One Bedroom Effective Rent/Unit`, `Two Bedroom Effective Rent/Unit`, `Three Bedroom Effective Rent/Unit`, `Four Bedroom Effective Rent/Unit`,
         `Vacancy %`,`For Sale Price`,`For Sale Status`,`For Sale Price`,`$Price/Unit`,`Latitude`,`Longitude`) %>%
  rename("Square Footage" = "RBA")

costar_all_props_clean


## Future rents based on historical rent growth

effective_rent_psf <- costar_ab_trend %>%
  filter(ab_props_period == "2022 YTD") %>%
  mutate(ab_props_effective_rent_per_sf = as.double(str_remove(ab_props_effective_rent_per_sf,"\\$"))) %>%
  pull(ab_props_effective_rent_per_sf)

average_unit_size <- costar_all_props %>%
  mutate(average_unit_size = sum((`Number Of Units`*`Avg Unit SF`),na.rm = T)/sum(`Number Of Units`,na.rm = T)) %>%
  slice_head() %>%
  pull(average_unit_size)

average_unit_size

average_annual_rent_growth <- costar_ab_trend %>%
  filter(ab_props_period %in% c("2022 YTD", "2021", "2020", "2019", "2018")) %>%
  mutate(ab_props_effective_rent_percent_growth_yr = as.double(str_remove(ab_props_effective_rent_percent_growth_yr,"%"))/100) %>%
  #group_by(ab_props_effective_rent_percent_growth_yr) %>%
  summarize(average = mean(ab_props_effective_rent_percent_growth_yr,na.rm = T)) %>%
  pull(average)



