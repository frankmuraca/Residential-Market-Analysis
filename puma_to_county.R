### PUMA-to-county methodology

### This R script produces county-level housing-related statistics using ACS microdata and HUD CHAS.


### Set working directory for data sources


### Library
library(tidyverse)
library(tidycensus)
library(patchwork)
library(here)
library(readxl)
library(tidytext)

### Read in PUMA to county list ==================================================
geocorr <- read_csv(here("data","puma_to_county.csv")) %>% 
  slice(-1) %>%
  mutate(cntyname = str_remove(cntyname," NC")) %>%
  group_by(puma12) %>%
  distinct(cntyname)

puma <- read_csv("puma_to_county.csv") %>% slice(-1) %>%
  mutate(cntyname = str_remove(cntyname," NC")) %>%
  filter(cntyname %in% mid_counties_list)


### Overlay Counties + PUMAs =======================

nc_pumas <- tigris::pumas(state = "NC")
nc_counties <- tigris::counties(state = "NC")

nc_counties %>% 
  filter(NAME %in% mid_counties_list) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = nc_pumas, fill = "NA", color = "firebrick") +
  theme_void()

nc_counties %>%
  ggplot() +
  geom_sf() +
  geom_label(data = mid_counties, aes(label = NAME, geometry = geometry),
                            stat = "sf_coordinates")



### LOAD AMI by Household Size ======================================


income_limits <- readxl::read_xlsx(here("data","hud_income_limits.xlsx")) %>%
  filter(State == 37) 

income_limits_clean <- income_limits %>%
  pivot_longer(cols = c(l50_1:l80_8)) %>%
  mutate(hh_size = as.factor(substr(name,5,5))) %>%
  mutate(income_abbr = substr(name, 1,3)) %>%
  mutate(ami = case_when(
    income_abbr == "ELI" ~ "30% AMI",
    income_abbr == "l50" ~ "50% AMI",
    income_abbr == "l80" ~ "80% AMI")) %>%
  select(County_Name,County,hh_size,ami, value) %>%
  mutate(county = str_remove(County_Name, " County"))

rm(income_limits)

puma_ami <- 
  geocorr %>%
  left_join(income_limits_clean, by = c("cntyname" = "county")) %>%
  pivot_wider(id_cols = c(cntyname,puma12,hh_size), names_from = ami, values_from = value) %>%
  group_by(puma12,hh_size) %>%
  summarise(`30% AMI` = mean(`30% AMI`,na.rm = T),
            `50% AMI` = mean(`50% AMI`,na.rm = T),
            `80% AMI` = mean(`80% AMI`,na.rm = T))

puma_ami

## Load in PUMS data

view(pums_variables)

pums_vars <- c("PUMA","TEN","HINCP","BLD")

pums <- get_pums(variables = pums_vars,
                 state = "NC",
                 recode = T)
table(pums$TEN_label)

pums_hh <- pums %>%
  group_by(SERIALNO) %>%
  summarize(weight = first(WGTP),
            puma = first(PUMA),
            hh_size = as.factor(n()),
            tenure = first(TEN_label),
            income = first(HINCP)) %>%
  filter(tenure != "N/A (GQ/vacant)")


### Merge PUMA data with AMI data

pums_hh_merge <- 
pums_hh %>%
  left_join(puma_ami, by = c("puma" = "puma12", "hh_size" = "hh_size")) %>%
  mutate(ami = case_when(
    income < `30% AMI` ~ "<30% AMI",
    income > `30% AMI` & income < `50% AMI` ~ "30-50% AMI",
    income > `50% AMI` & income < `80% AMI` ~ "50-80% AMI",
    income > `80% AMI` ~ "80%< AMI"
  ))




## Get census data for weighting ============

vars <- load_variables(year = 2019, dataset  = "acs5")


### CHAS ==================================================

### Table 15C =============================================
dictionary_tbl15C <- read_excel("CHAS_Counties/CHAS data dictionary 13-17.xlsx",
                                sheet = "Table 15C") ## <---- Contains variable definitions

tbl15C_raw <- read_csv("CHAS_Counties/Table15C.csv") ## <- Contains variable values

tbl15C_raw_long <- tbl15C_raw %>% ### Subset to NC counties 
  filter(grepl("North Carolina",name)) 

tbl15C_est <- tbl15C_raw_long %>% ### Pivot county names, estimate variables, and estimates to long format
  select(name,contains("est")) %>% pivot_longer(cols = starts_with("T15"),values_to = "count",names_repair = "unique")

tbl15C_moe <- tbl15C_raw_long %>% ### Pivot margin of error values
  select(contains("moe")) %>% pivot_longer(cols = starts_with("T15"),values_to = "MOE")

tbl15C_merge <- cbind(tbl15C_est,tbl15C_moe) ### combine counts and MOEs

tbl15C_merge <- tbl15C_merge %>% ### rename dataframe
  rename(county = "name...1",
         estimate_label = "name...2",
         moe_label = "name")

tbl15C_merge <- merge(tbl15C_merge,dictionary_tbl15C, #### merge variable definitions with variable values
                      by.x="estimate_label",
                      by.y="Column Name") %>%
  mutate(county = str_remove(county," County, North Carolina")) %>%
  left_join(geocorr, by = c("county" = "cntyname")) %>%
  mutate(`Rent` = recode(`Rent`, #Rename variables
                         `greater than RHUD30 but less than or equal to RHUD50` = "Units affordable to 30-50% AMI",
                         `greater than RHUD50 but less than or equal to RHUD80` = "Units affordable to 50-80% AMI",
                         `greater than RHUD80` = "Units affordable to 80%< AMI",
                         `less than or equal to RHUD30` = "Units affordable to <30% AMI"),
         `Rent` = fct_relevel(as.factor(`Rent`), # Change order of values for visualizations
                              "Units affordable to 80%< AMI",
                              "Units affordable to 50-80% AMI",
                              "Units affordable to 30-50% AMI",
                              "Units affordable to <30% AMI"),
         `Household income` = recode(`Household income`, #Rename variables
                                     `greater than 100% of HAMFI` = "100%< AMI",
                                     `greater than 30% but less than or equal to 50% of HAMFI` = "30-50% AMI",
                                     `greater than 80% but less than or equal to 100% of HAMFI` = "80-100% AMI",
                                     `less than or equal to 30% of HAMFI` = "<30% AMI",
                                     `greater than 50% but less than or equal to 80% of HAMFI` = "50-80% AMI"),
         `Household income` = fct_relevel(as.factor(`Household income`), #Change order of values
                                          "100%< AMI",
                                          "80-100% AMI",
                                          "50-80% AMI",
                                          "30-50% AMI",
                                          "<30% AMI"))

### Count proportions by groups of counties

under_30_ami <- tbl15C_merge %>%
  filter(Line_Type == "Detail") %>%
  filter(`Household income` == "<30% AMI") %>%
  group_by(puma12,county) %>%
  summarise(total = sum(count,na.rm=T)) %>%
  mutate(pct = total/sum(total)) %>%
  mutate(income = "<30% AMI")






## Table 7 -----------------------------
dictionary_tbl7 <- read_excel(here("data","2014thru2018-050-csv","050","CHAS data dictionary 14-18.xlsx"),
                              sheet = "Table 7") ## <---- Contains variable definitions

tbl7_raw <- read_csv(here("data","2014thru2018-050-csv","050","Table7.csv")) %>%  ## <- Contains variable values
  filter(grepl("North Carolina",name)) 

tbl7_est <- tbl7_raw %>% ### Pivot county names, estimate variables, and estimates to long format
  select(name,contains("est")) %>% pivot_longer(cols = starts_with("T7"),values_to = "count",names_repair = "unique")

tbl7_moe <- tbl7_raw %>% ### Pivot margin of error values
  select(contains("moe")) %>% pivot_longer(cols = starts_with("T7"),values_to = "MOE")

tbl7_merge <- cbind(tbl7_est,tbl7_moe) ### combine counts and MOEs

tbl7_merge <- tbl7_merge %>% ### rename dataframe
  rename(county = "name...1",
         estimate_label = "name...2",
         moe_label = "name")

tbl7_merge <- merge(tbl7_merge,dictionary_tbl7, #### merge variable definitions with variable values
                    by.x="estimate_label",
                    by.y="Column Name")

## Calculate proportions of tenure, income, and cost burden by PUMA

counties <- c("Wilson County, North Carolina","Greene County North Carolina")

counties_tbl7 <- tbl7_merge %>% filter(county %in% counties)

rm(tbl7_merge)

counties_tbl7 %>%
  filter(Line_Type == "Detail") %>%
  group_by(Tenure,`Household income`,)
