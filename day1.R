library(tidyverse)

calories <- readLines("input1.txt")
calories <- as.numeric(calories)

part1 <- tibble(calories = calories, elf = 0) %>%
  mutate(elf = ifelse(is.na(calories),1,0),
         elf = cumsum(elf)) %>%
  group_by(elf) %>%
  summarise(calories = sum(calories, na.rm = T)) %>%
  arrange(-calories)
part1 %>% slice(1)

#Part 2
part1 %>%
  slice(1:3) %>%
  summarise(calories = sum(calories, na.rm = T))
