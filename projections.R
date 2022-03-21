library(tidyverse)
library(here)

special_tabs <- read_csv(here("data","hud_special-tabs_2019.csv"))

esri_bao <- read_csv(here("data","esri_bao.csv"))


tenure_income_proportions  <- special_tabs %>%
  pivot_longer(cols = c(`1-person`:`4+ persons`), names_to = "hh_size",values_to = "count") %>%
  pivot_wider(id_cols = c(Tenure,Age,hh_size),names_from = income_bracket,values_from = count) %>%

## Create new income brackets 
  mutate(`Less than $15,000` = `Less than $9,999` + (`$10,000 - $19,999`)/2) %>%
  mutate(`$15,000 - $24,999` = (`$10,000 - $19,999`/2) + (`$20,000 - $29,999`/2)) %>%
  mutate(`$25,000 - $34,999` = (`$20,000 - $29,999`/2) + (`$30,000 - $39,999`/2)) %>%
  mutate(`$35,000 - $49,999` = (`$30,000 - $39,999`/2) + (`$40,000 - $49,999`)) %>%
  mutate(`$50,000 - $74,999` = `$50,000 - $59,999` + `$60,000 - $74,999`) %>%
    
## Drop old income brackets
  select(-c(`Less than $9,999`, `$10,000 - $19,999`, `$20,000 - $29,999`, `$30,000 - $39,999`, `$40,000 - $49,999`, `$50,000 - $59,999`,`$60,000 - $74,999`)) %>%
    
## Create tenure proportions by age and income bracket (dropping hh_size variable)
  pivot_longer(cols = -c(Tenure,Age,hh_size), names_to = "income_bracket", values_to = "count") %>%
  group_by(Age,income_bracket,Tenure) %>%
  summarize(count = sum(count,na.rm = T)) %>%
  mutate(pct = count/sum(count)) %>%
  pivot_wider(id_cols = c(Age,income_bracket), names_from = Tenure, values_from = pct)

## Wrangle esri data so it can merge with tabs data

projections <- 
  esri_bao %>% 
  filter(grepl("Householder Age", Variable)) %>%
  filter(!grepl("Aggregate", Variable)) %>%
  
## Break out age brackets from variable name 
  mutate(esri_age_bracket = case_when(
    grepl("15-24",Variable) ~ "15-24",
    grepl("25-34",Variable) ~ "25-34",
    grepl("35-44",Variable) ~ "35-44",
    grepl("45-54",Variable) ~ "45-54",
    grepl("55-64",Variable) ~ "55-64",
    grepl("65-74",Variable) ~ "65-74",
    grepl("75+",Variable) ~ "75+"
  )) %>%
  
## Break out income brackets from variable name 

  mutate(esri_income_bracket = case_when(
    grepl("< [$]15,000",Variable) ~ "Less than $15,000",
    grepl("[$]15,000-[$]24,999",Variable) ~ "$15,000 - $24,999",
    grepl("[$]25,000-[$]34,999",Variable) ~ "$25,000 - $34,999",
    grepl("[$]35,000-[$]49,999",Variable) ~ "$35,000 - $49,999",
    grepl("[$]50,000-[$]74,999",Variable) ~ "$50,000 - $74,999",
    grepl("[$]75,000-[$]99,999",Variable) ~ "$75,000 - $99,999",
    grepl("[$]100,000-[$]149,999",Variable) ~ "$100,000 - $149,999",
    grepl("[$]150,000-[$]199,999",Variable) ~ "$150,000 - $199,999",
    grepl("[$]200,000+",Variable) ~ "$200,000+"
  )) %>% 

## Break out year from variable names   
  mutate(year = case_when(
    grepl("2021",Variable) ~ "2021",
    grepl("2026",Variable) ~ "2026"
  )) %>%

## Only select market area by column position, age bracket, income bracket, and year    
  select(2,esri_age_bracket,esri_income_bracket,year) %>%
  
## Clean up count  
  mutate(count = as.double(str_remove(.[[1]],","))) %>%

## Set income bracket as columns to convert them to income brackets shown in Special Tabulations  
  pivot_wider(id_cols = c(year,esri_age_bracket), names_from = esri_income_bracket, values_from = count) %>%
  mutate(`$100,000 - $124,999` = `$100,000 - $149,999`/2) %>%
  mutate(`$125,000 or more` = (`$100,000 - $149,999`/2) + `$150,000 - $199,999` + `$200,000+`) %>%
  
## Remove unused income brackets   
  select(-c(`$100,000 - $149,999`, `$150,000 - $199,999`, `$200,000+`)) %>%
  
## Wrangle data back so that columns reflect different years  
  pivot_longer(cols = -c(year, esri_age_bracket), names_to = "tabs_income_bracket", values_to = "count") %>%
  pivot_wider(id_cols = c(esri_age_bracket,tabs_income_bracket), names_from = year, values_from = count) %>%
  mutate(change = `2026` - `2021`) %>%
  
## Make variable of Special tabulation age brackets  
  mutate(tabs_age_bracket = case_when(
    esri_age_bracket %in% c("15-24", "25-34", "35-44","45-54", "55-64") ~ "15 - 61",
    esri_age_bracket %in% c("65-74", "75+") ~ "62+"
  )) %>%
  
## Merge proportions of special tabs by age and income   
  left_join(tenure_income_proportions, by = c("tabs_age_bracket" = "Age", "tabs_income_bracket" = "income_bracket")) %>%
 
## Project number of renters and owners by current proportions  
   mutate(renters_2026 = change*Renter) %>%
  mutate(owners_2026 = change*Owner) %>%
  
## Create variable for low and high ranges. Used later when calculating percentage of renters that can afford market rate
  
  mutate(range_low = case_when(
    tabs_income_bracket == "Less than $15,000" ~ 0,
    tabs_income_bracket == "$15,000 - $24,999" ~ 15000,
    tabs_income_bracket == "$25,000 - $34,999" ~ 25000,
    tabs_income_bracket == "$35,000 - $49,999" ~ 35000,
    tabs_income_bracket == "$50,000 - $74,999" ~ 50000,
    tabs_income_bracket == "$75,000 - $99,999" ~ 75000,
    tabs_income_bracket == "$100,000 - $124,999" ~ 100000,
    tabs_income_bracket == "$125,000+" ~ 125000
  )) %>%
  mutate(range_high = case_when(
    tabs_income_bracket == "Less than $15,000" ~ 15000,
    tabs_income_bracket == "$15,000 - $24,999" ~ 24999,
    tabs_income_bracket == "$25,000 - $34,999" ~ 34999,
    tabs_income_bracket == "$35,000 - $49,999" ~ 49999,
    tabs_income_bracket == "$50,000 - $74,999" ~ 74999,
    tabs_income_bracket == "$75,000 - $99,999" ~ 99999,
    tabs_income_bracket == "$100,000 - $124,999" ~ 124999
    #tabs_income_bracket == "$125,000+" 
  )) %>%
  mutate(range = case_when(
    tabs_income_bracket == "Less than $15,000" ~ 15000,
    tabs_income_bracket == "$15,000 - $24,999" ~ 9999,
    tabs_income_bracket == "$25,000 - $34,999" ~ 9999,
    tabs_income_bracket == "$35,000 - $49,999" ~ 14999,
    tabs_income_bracket == "$50,000 - $74,999" ~ 24999,
    tabs_income_bracket == "$75,000 - $99,999" ~ 24999,
    tabs_income_bracket == "$100,000 - $124,999" ~ 24999
    #tabs_income_bracket == "$125,000+" 
  )) 



## Generate tabls  

income_order <- c("Less than $15,000", "$15,000 - $24,999", "$25,000 - $34,999", "$35,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999", "$100,000 - $124,999", "$125,000 or more")

income_projections <- projections %>%
  group_by(tabs_income_bracket) %>%
  summarize(Renters = round(sum(renters_2026, na.rm = T)),
            Owners = round(sum(owners_2026, na.rm = T))) %>%
  rename("Income Bracket" = tabs_income_bracket) %>%
  slice(match(income_order, `Income Bracket`)) 

income_can_afford <- income_projections %>%
  mutate(range_low = case_when(
    `Income Bracket` == "Less than $15,000" ~ 0,
    `Income Bracket` == "$15,000 - $24,999" ~ 15000,
    `Income Bracket` == "$25,000 - $34,999" ~ 25000,
    `Income Bracket` == "$35,000 - $49,999" ~ 35000,
    `Income Bracket` == "$50,000 - $74,999" ~ 50000,
    `Income Bracket` == "$75,000 - $99,999" ~ 75000,
    `Income Bracket` == "$100,000 - $124,999" ~ 100000,
    `Income Bracket` == "$125,000 or more" ~ 125000
  )) %>%
  mutate(range_high = case_when(
    `Income Bracket` == "Less than $15,000" ~ 15000,
    `Income Bracket` == "$15,000 - $24,999" ~ 24999,
    `Income Bracket` == "$25,000 - $34,999" ~ 34999,
    `Income Bracket` == "$35,000 - $49,999" ~ 49999,
    `Income Bracket` == "$50,000 - $74,999" ~ 74999,
    `Income Bracket` == "$75,000 - $99,999" ~ 99999,
    `Income Bracket` == "$100,000 - $124,999" ~ 124999,
    `Income Bracket` == "$125,000 or more" ~ 300000
  )) %>%
  mutate(range = case_when(
    `Income Bracket` == "Less than $15,000" ~ 15000,
    `Income Bracket` == "$15,000 - $24,999" ~ 9999,
    `Income Bracket` == "$25,000 - $34,999" ~ 9999,
    `Income Bracket` == "$35,000 - $49,999" ~ 14999,
    `Income Bracket` == "$50,000 - $74,999" ~ 24999,
    `Income Bracket` == "$75,000 - $99,999" ~ 24999,
    `Income Bracket` == "$100,000 - $124,999" ~ 24999
    #tabs_income_bracket == "$125,000 or more" 
  )) 


