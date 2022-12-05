library(tidyverse)
rucksack <- readLines("input3.txt")
data <- tibble(rucksack = rucksack) %>%
  mutate(nchar = nchar(rucksack))

priorities <- tibble(item = c(letters, LETTERS), value = 1:52)

data <- data %>%
  mutate(c1 = str_sub(rucksack, start = 1L, end = nchar/2),
         c2 = str_sub(rucksack, start = 1+(nchar/2), end = nchar),
         pattern = str_c("[",c2,"]"),
         item = str_extract(c1, pattern)) %>%
  left_join(priorities)

data %>%
  summarise(value = sum(value, na.rm =T))

#Part two

data <- data %>%
  select(rucksack) %>%
  mutate(group = sort(rep(1:(nrow(.)/3),3)))

myfun <- function(x){
  step1 <- str_extract_all(x[1], str_c("[",x[2],"]"))[[1]]
  step2 <- str_extract(step1, str_c("[",x[3],"]"))
  return(step2[!is.na(step2)])
}

data %>%
  group_by(group) %>%
  summarise(item = myfun(rucksack)) %>%
  ungroup() %>%
  distinct() %>%
  left_join(priorities) %>%
  summarise(value = sum(value, na.rm =T))

