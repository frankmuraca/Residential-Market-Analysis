library(arrow)
library(here)
library(ggplot2)


counties <- read_parquet(here("data","counties_annual.parquet")) %>% filter(fips_state == 37)
table(counties$year)

counties %>% 
  filter(name == "Wilson County, NC") %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = total_units, group = name)) +
  geom_line()


table(counties$year)
