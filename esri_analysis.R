
esri_bao <- read_csv(here("data","esri_bao.csv"))


summary_table <- esri_bao %>%
  select(1:2) %>%
  filter(Variable %in% c("2021 Total Population",
                         "2026 Total Population",
                         "2021 Total Households",
                         "2026 Total Households",
                         "2021 Median Household Income",
                         "2026 Median Household Income")) %>%
  mutate(Year = case_when(
    grepl("2021",Variable) ~ "2021",
    grepl("2026",Variable) ~ "2026"
  )) %>%
  mutate(Variable = substring(Variable,6)) %>%
  pivot_wider(id_cols = Variable, names_from = "Year", values_from = 2) %>%
  arrange(desc(Variable)) %>%
  select(Variable, `2021`, `2026`) %>%
  mutate(dummy_2021 = str_remove(`2021`,"[$]")) %>%
  mutate(dummy_2021 = as.double(str_remove(dummy_2021,","))) %>%
  mutate(dummy_2026 = str_remove(`2026`,"[$]")) %>%
  mutate(dummy_2026 = as.double(str_remove(dummy_2026,","))) %>%
  mutate(Change = dummy_2026 - dummy_2021) %>%
  select(-c(dummy_2021,dummy_2026))
