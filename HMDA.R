## HMDA for Residential Market Analysis
### This R script merges HMDA data from 2007-2017, 2018, and 2019 to show homeownership trends in primary
### and secondary market areas based on Census Tract or County boundaries. 

### The data is used in residential market analysis baseline materials to estimate how much each income bracket is willing to spend 
### home purchases. The R script filters the data by geography, but additional filters are required 

### Download the 2018 and 2019 files from this link: https://ffiec.cfpb.gov/data-browser/data/2019?category=states
### Use only the "state" filter and select North Carolina to avoid downloading super large files. 


## Libraries
library(tidyverse)



## Read in datasets, select relevant columns, rename columns ===================
### HMDA 2018, 2019

hmda_2018 <- read_csv("C:/Users/fmuraca/Dropbox (DFI)/DFI Resources/Analyst Materials/Data + Methodology/Affordable Housing/HMDA/state_NC_2018.csv")
hmda_2019 <- read_csv("C:/Users/fmuraca/Dropbox (DFI)/DFI Resources/Analyst Materials/Data + Methodology/Affordable Housing/HMDA/state_NC_2019.csv")
hmda_2020 <- read_csv("C:/Users/fmuraca/Dropbox (DFI)/DFI Resources/Analyst Materials/Data + Methodology/Affordable Housing/HMDA/state_NC_2020.csv")

hmda <- rbind(hmda_2018,hmda_2019,hmda_2020) # merge 2018 & 2019


#### Set Census tracts for Market Area =====================================
#### In this instance, the primary market area is made up of Census Tract numbers, but could also be 
#### County FIPS codes as is the case for the secondary market area.

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


### Clean merged data by converting income from "thousands of dollars" and adding income and property value brackets.

hmda_clean <- hmda %>%
  mutate(income = income*1000) %>% # convert from "thousands of dollars"
  mutate(income_bracket = case_when( # income brackets identical to ESRI BAO income brackets
    income < 15000 ~ "Less than $15,000",
    income >= 15000 & income < 24999 ~ "$15,000-$25,000",
    income >= 25000 & income < 34999 ~ "$25,000-$35,000",
    income >= 35000 & income < 49999 ~ "$35,000-$50,000",
    income >= 50000 & income < 74999 ~ "$50,000-$75,000",
    income >= 75000 & income < 99999 ~ "$75,000-$100,000",
    income >= 100000 & income < 124999 ~ "$100,000-$124,999",
    income >= 125000 ~ "Greater than $125,000"
  )) %>%
  mutate(property_bracket = case_when( # Property value brackets can be altered to provide more granular detail. In thousands of dollars.
    property_value < 150000 ~ "Less than $150",
    property_value >= 150000 & property_value < 175000 ~ "$150-$175",
    property_value >= 175000 & property_value < 200000 ~ "$175-$200",
    property_value >= 200000 & property_value < 300000 ~ "$200-$300",
    property_value >= 300000 & property_value < 400000 ~ "$300-$400",
    property_value >= 400000 & property_value < 500000 ~ "$400-$500",
    property_value >= 500000 & property_value < 750000 ~ "$500-$750",
    property_value >= 750000 & property_value < 1000000 ~ "$750-$1,000",
    property_value >= 1000000 & property_value < 1500000 ~ "$1,000-$1,500",
    property_value >= 1500000 ~ "Greater than $1,500"))

hmda_clean %>% filter(is.na(property_bracket) & !is.na(property_value)) %>% select(property_value,property_bracket)
#### Subset data by primary market area
hmda_clean %>%
  filter(census_tract %in% market_area_tracts) %>% ## primary market area is currently defined by census tracts. Could be altered to county fips
  write_csv("hmda_primary_market.csv")


#### Subset data by secondary market area
hmda_2018_2019_clean %>%
  filter(county_code %in% secondary_market_area) %>% ## secondary market area is currently defined by counties. Could be altered to census tracts
  write_csv("hmda_secondary_market.csv")



###### APPENDIX FOR FUTURE WORK =============================================================================================
##### =======================================================================================================================

#### Primary market area =====================
hmda_2018_2019 %>%
  #filter(census_tract %in% market_area_tracts) %>%
  filter(county_code %in% market_area_county) %>%
  #filter(action_taken == 1) %>%
  #filter(derived_dwelling_category == "Single Family (1-4 Units):Site-Built") %>%
  #filter(loan_type == 1) %>%
  #filter(loan_purpose == 1) %>%
  #filter(business_or_commercial_purpose == 2) %>%
  #filter(occupancy_type == 1) %>%
  #filter(derived_loan_product_type == "Conventional:First Lien") %>%
  mutate(census_property_value = case_when(
    property_value < 50000 ~ "Less than $50,000",
    property_value >= 50000 & property_value < 99999 ~ "$50,000-$99,999",
    property_value >= 99999 & property_value < 149999 ~ "$100,000-$149,999",
    property_value >= 149999 & property_value < 199999 ~ "$150,000-$199,999",
    property_value >= 199999 & property_value < 249999 ~ "$200,000-$249,999",
    property_value >= 249999 & property_value < 299999 ~ "$250,000-$299,999",
    property_value >= 299999 & property_value < 399999 ~ "$300,000-$399,999",
    property_value >= 399999 & property_value < 499999 ~ "$400,000-$499,999",
    property_value >= 499999 & property_value < 749999 ~ "$500,000-$749,999",
    property_value >= 749999 & property_value < 999999 ~ "$750,000-$999,999",
    property_value >= 999999 & property_value < 1499999 ~ "$1,000,000-$1,499,999",
    property_value >= 1499999 & property_value < 1999999 ~ "$1,500,000-$1,999,999",
    property_value >= 1999999 ~ "$2,000,000 or greater" 
  )) %>%
  mutate(census_property_value = fct_relevel(census_property_value,
                                             levels = c(
                                               "Less than $50,000",
                                               "$50,000-$99,999",
                                              "$100,000-$149,999",
                                               "$150,000-$199,999",
                                               "$200,000-$249,999",
                                               "$250,000-$299,999",
                                              "$300,000-$399,999",
                                              "$400,000-$499,999",
                                              "$500,000-$749,999",
                                               "$750,000-$999,999",
                                               "$1,000,000-$1,499,999",
                                               "$1,500,000-$1,999,999",
                                              "$2,000,000 or greater" 
                                             )))
  write_csv("sp_2nd_hmda_2018_2019.csv")


hmda_2019_clean <- hmda_2019 %>%
  filter(county_code == "181" | census_tract %in% market_area_tracts) %>%
  filter(derived_dwelling_category == "Single Family (1-4 Units):Site-Built", # SIngle Family
         derived_loan_product_type == "Conventional:First Lien", # Conventional
         loan_purpose == 1) %>% # Home purchase 
  mutate(income = income)

hmda_2017_2010 <- hmda_all %>%
  filter(county_code == "181" | census_tract_number %in% market_area_tracts) %>%
  filter(property_type == 1, # Single Family
         loan_type_name == "Conventional", # Conventional
    loan_purpose == 1) %>% # Home purchase
  mutate(income = applicant_income_000s*1000) %>%
  select(as_of_year,action_taken,income,census_tract_number,)
