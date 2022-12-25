library(tidyverse)

data <- tibble(instruction = readLines("input10.txt"))
data <- data %>% 
  mutate(instruction = ifelse(grepl("^noop", instruction), "noop 0", instruction)) %>% 
  separate(instruction, c("instruction", "value"), sep = " ") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(cycle = ifelse(instruction == "noop", 1, 2)) %>% 
  mutate(cumcycle = cumsum(cycle))

data <- data %>% 
  uncount(cycle) %>% 
  mutate(cycle = 1:nrow(.)) %>% 
  mutate(value = ifelse(cycle == cumcycle,value,0)) %>% 
  select(-cumcycle) 

data <- bind_rows(tibble(instruction = "start", value = 1, cycle = 0), data) %>%
  mutate(X = cumsum(value))

data <- data %>% 
  mutate(during_cycle = dplyr::lag(X,1))

data %>% 
  filter(cycle == 20 | (cycle-20)%%40 == 0) %>% 
  mutate(signal_strength = cycle * during_cycle) %>% 
  summarise(total = sum(signal_strength, na.rm = T))

#Part 2
data %>% 
  slice(1:(nrow(.)-1)) %>% 
  mutate(pos = rep(0:39, nrow(.)%/%40)) %>% 
  mutate(pixel = ifelse(pos == X-1 |pos == X | pos == X+1, "#", ".")) %>% 
  mutate(row = cycle %/% 40) %>% 
  group_by(row) %>% 
  summarise(pixel = paste0(pixel, collapse = ""))
  