library(tidycensus)
library(dplyr)
library(stringr)
library(tidyr)


market_area_tracts <- c('37065020200',
                        '37065020300',
                        '37065020400',
                        '37065021400',
                        '37101040201',
                        '37127010200',
                        '37127010300',
                        '37127010400',
                        '37127010502',
                        '37127010503',
                        '37127010504',
                        '37127011000',
                        '37127011101',
                        '37127011102',
                        '37127011200',
                        '37127011300',
                        '37127011400',
                        '37127011500',
                        '37195000100',
                        '37195000200',
                        '37195000300',
                        '37195000400',
                        '37195000501',
                        '37195000502',
                        '37195000600',
                        '37195000700',
                        '37195000801',
                        '37195000802',
                        '37195000900',
                        '37195001000',
                        '37195001100',
                        '37195001200',
                        '37195001300',
                        '37195001400',
                        '37195001500',
                        '37195001600',
                        '37195001700'
                        )



vars <- load_variables(year = 2019, dataset = "acs5")





tenure_mobility_age_code <- vars %>% filter(concept == "TENURE BY AGE OF HOUSEHOLDER BY YEAR HOUSEHOLDER MOVED INTO UNIT") %>% pull(name)

## Tenure ===========================================
## ==================================================

tenure_code <- vars %>% filter(concept == "TENURE") %>% pull(name)


tenure <- NULL

for(i in seq(2010,2019)){
  
  tmp <- get_acs(year = i,
                 geography = "tract",
                 state = "NC",
                 variables = tenure_code
                 )
  tmp$year <- i
  
  tenure <- rbind(tenure, tmp)
  
}

tenure_clean <- tenure %>%
  filter(GEOID %in% market_area_tracts) %>%
  left_join(vars %>% filter(concept == "TENURE"), by = c("variable" = "name")) %>%
  filter(label != "Estimate!!Total:") %>%
  mutate(label_clean = str_remove(label, "Estimate!!Total:!!"))

tenure_clean %>%
  group_by(year,label_clean) %>%
  summarize(total = sum(estimate,na.rm = T)) %>%
  pivot_wider(id_cols = year, names_from = label_clean, values_from = total) %>%
  DataEditR::data_edit()

## Tenure by Units in Structure ===============================================
## ============================================================================

tenure_structure_code <- vars %>% filter(concept == "TENURE BY UNITS IN STRUCTURE") %>% pull(name)


tenure_structure <- NULL

for(i in seq(2010,2019)){
  
  tmp <- get_acs(year = i,
                 geography = "tract",
                 state = "NC",
                 variables = tenure_structure_code
  )
  tmp$year <- i
  
  tenure_structure <- rbind(tenure_structure, tmp)
  
}

tenure_structure_clean <- 
  tenure_structure %>%
  filter(GEOID %in% market_area_tracts) %>%
  left_join(vars %>% filter(concept == "TENURE BY UNITS IN STRUCTURE"), by = c("variable" = "name")) %>%
  filter(!label %in% c("Estimate!!Total:","Estimate!!Total:!!Owner-occupied housing units:","Estimate!!Total:!!Renter-occupied housing units:")) %>%
  mutate(label_clean = str_remove(label, "Estimate!!Total:!!")) %>%
    mutate(tenure = case_when(
      grepl("Owner",label_clean) ~ "Owner",
      grepl("Renter",label_clean) ~ "Renter"
    )) %>%
    mutate(label_clean = str_remove(label_clean,"Owner-occupied housing units:!!")) %>%
    mutate(label_clean = str_remove(label_clean,"Renter-occupied housing units:!!")) %>%
    select(-c(label,concept))

tenure_structure_clean %>%
  group_by(tenure,label_clean,year) %>%
  summarize(total = sum(estimate,na.rm = T)) %>%
  pivot_wider(id_cols = c(year,label_clean), names_from = tenure, values_from = total) %>%
  DataEditR::data_edit()

## Tenure by Unit Age =========================================================
## ============================================================================

tenure_unitage_code <- vars %>% filter(concept == "TENURE BY YEAR STRUCTURE BUILT") %>% pull(name)

tenure_unitage <- NULL

for(i in seq(2015,2019)){
  
  tmp <- get_acs(year = i,
                 geography = "tract",
                 state = "NC",
                 variables = tenure_unitage_code
  )
  tmp$year <- i
  
  tenure_unitage <- rbind(tenure_unitage, tmp)
  
}

tenure_unitage_clean <- 
  tenure_unitage %>%
  filter(GEOID %in% market_area_tracts) %>%
  left_join(vars %>% filter(concept == "TENURE BY YEAR STRUCTURE BUILT"), by = c("variable" = "name")) %>%
    filter(!label %in% c("Estimate!!Total:","Estimate!!Total:!!Owner occupied:","Estimate!!Total:!!Renter occupied:")) %>%
    mutate(label_clean = str_remove(label, "Estimate!!Total:!!")) %>%
    mutate(tenure = case_when(
      grepl("Owner",label_clean) ~ "Owner",
      grepl("Renter",label_clean) ~ "Renter"
    )) %>%
    mutate(label_clean = str_remove(label_clean,"Owner occupied:!!")) %>%
    mutate(label_clean = str_remove(label_clean,"Renter occupied:!!")) %>%
    select(-c(label,concept))
  
tenure_unitage_clean %>%
  group_by(tenure,label_clean,year) %>%
  summarize(total = sum(estimate,na.rm = T)) %>%
  pivot_wider(id_cols = c(year,label_clean), names_from = tenure, values_from = total) %>%
  DataEditR::data_edit()


## Tenure income ====================================
## ==================================================


tenure_income_code <- vars %>% filter(concept == "TENURE BY HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)") %>% pull(name)


tenure_income <- NULL

for(i in seq(2010,2019)){
  
  tmp <- get_acs(year = i,
                 geography = "tract",
                 state = "NC",
                 variables = tenure_income_code
  )
  tmp$year <- i
  
  tenure_income <- rbind(tenure_income, tmp)
  
  rm(tmp)
  
}


tenure_income_clean <- 
  tenure_income %>%
  filter(GEOID %in% market_area_tracts) %>%
  left_join(vars %>% filter(concept == "TENURE BY HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)"), by = c("variable" = "name")) %>%
  filter(!label %in% c("Estimate!!Total:","Estimate!!Total:!!Owner occupied:","Estimate!!Total:!!Renter occupied:")) %>%
  mutate(label_clean = str_remove(label, "Estimate!!Total:!!")) %>%
  mutate(tenure = case_when(
    grepl("Owner",label_clean) ~ "Owner",
    grepl("Renter",label_clean) ~ "Renter"
  )) %>%
  mutate(label_clean = str_remove(label_clean,"Owner occupied:!!")) %>%
  mutate(label_clean = str_remove(label_clean,"Renter occupied:!!")) %>%
  select(-c(label,concept))

tenure_income_clean %>%
 filter(tenure == "Renter") %>%
  group_by(GEOID,year,label_clean) %>%
  mutate(total = sum(estimate,na.rm = T)) %>%
  ggplot(aes(x = year, y = estimate, group = label_clean)) +
  geom_col() +
  facet_wrap(~label_clean)
  


### Mobility by Tenure ======================
## 

tenure_mobility_code <- vars %>% filter(concept == "TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT") %>% pull(name)

tenure_mobility <- NULL

for(i in seq(2010,2019)){
  
  tmp <- get_acs(year = i,
                 geography = "tract",
                 state = "NC",
                 variables = tenure_mobility_code
  )
  tmp$year <- i
  
  tenure_mobility <- rbind(tenure_mobility, tmp)
  
  rm(tmp)
  
}

tenure_mobility_clean <- 
  tenure_mobility %>%
  filter(GEOID %in% market_area_tracts) %>%
  left_join(vars %>% filter(concept == "TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT"), by = c("variable" = "name")) %>%
  filter(!label %in% c("Estimate!!Total:","Estimate!!Total:!!Owner occupied:","Estimate!!Total:!!Renter occupied:")) %>%
  mutate(label_clean = str_remove(label, "Estimate!!Total:!!")) %>%
    mutate(tenure = case_when(
      grepl("Owner",label_clean) ~ "Owner",
      grepl("Renter",label_clean) ~ "Renter"
    )) %>%
    mutate(label_clean = str_remove(label_clean,"Owner occupied:!!")) %>%
    mutate(label_clean = str_remove(label_clean,"Renter occupied:!!")) %>%
    select(-c(label,concept))

tenure_mobility_clean %>%
  filter(tenure == "Renter") %>%
  filter(year == 2019) %>%
  group_by(year, label_clean) %>%
  summarize(total = sum(estimate,na.rm = T)) %>%
  ggplot(aes(x = label_clean, y = total)) +
  geom_col() 
  