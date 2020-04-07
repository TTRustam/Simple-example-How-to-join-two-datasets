# -------------------------------------------------------------------- # 
library("tidyverse")

# Dataframes to be joined
table1 <- tibble(age     = c("00-04", "05-09", "10-14"),
                 deaths  = rpois(3, lambda = 10),
                 sex     = c("m", "f", "m"),
                 country = c("One", "One", "Two"))

# Second
table2 <- tibble(age     = c(0:14),
                 deaths  = rpois(15, lambda = 10),
                 sex     = c("m", "f", "m", "m", "f", "m","f", "m", "f", "f", "m", "f", "m", "f", "m"),
                 country = c("One", "One", "Two", "One", "Two", "One", "One",  
                             "Two", "Two", "One", "Two", "Two", "Two", "One", "Two")) %>% 
  mutate(age_group = case_when(
    between(age, 0, 4)   ~ "00-04",
    between(age, 5, 9)   ~ "05-09",
    between(age, 10, 14) ~ "10-14"
  )) 

# -------------------------------------------------------------------- # 
# Option one
table3 <- table1 %>% 
  left_join(table2, by = c("country", "age" = "age_group"))

# -------------------------------------------------------------------- # 
# Option two + map2 use example
tab_2_gr <- table2 %>%
  group_by(country, age_group) %>%
  nest() %>%
  ungroup() 

# NOTE: any inconsistencies in new_age because of groupping interaction with country, my bad choice)
# in your data this effect will not be present due to obvious reasons
table1 %>% 
  left_join(tab_2_gr, by = c("country", "age" = "age_group")) %>% 
  mutate(data_3 = map2(deaths, data, ~ .x * 2 / 16 + pull(.y, deaths)), 
         data   = map(data,          ~ .x %>% dplyr::select(new_age = age))) %>% 
  dplyr::select(age, sex, country, data, data_3) %>% 
  unnest(cols = c(data, data_3))
# -------------------------------------------------------------------- # 