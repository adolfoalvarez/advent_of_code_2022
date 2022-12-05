library(tidyverse)
data <- read_delim("input2.txt", delim = " ", col_names =c ("opp", "me"))
data %>%
  mutate(shape = case_when(
    me == "X" ~ 1,
    me == "Y" ~ 2,
    TRUE ~ 3
  )) %>%
  unite("oppme", opp,me, sep = "") %>%
  mutate(outcome = case_when(
    oppme %in% c("CX", "AY", "BZ") ~ 6,
    oppme %in% c("BX", "CY", "AZ") ~ 0,
    TRUE ~ 3
  )) %>%
  mutate(total = shape + outcome) %>%
  summarise(total = sum(total))

#Part two

data %>%
  mutate(outcome = case_when(
    me == "X" ~ 0,
    me == "Y" ~ 3,
    TRUE ~ 6
  )) %>%
  unite("oppme", opp,me, sep = "") %>%
  mutate(shape = case_when(
    oppme %in% c("AY", "BX", "CZ") ~ 1,
    oppme %in% c("AZ", "BY", "CX") ~ 2,
    TRUE ~ 3
  )) %>%
  mutate(total = shape + outcome) %>%
  summarise(total = sum(total))

