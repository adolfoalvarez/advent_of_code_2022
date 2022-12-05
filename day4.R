library(tidyverse)
pairs <- read_delim("input4.txt", delim = ",", col_names = c("p1", "p2"))
pairs <- pairs %>%
  separate(p1, into = c("p11", "p12")) %>%
  separate(p2, into = c("p21", "p22")) %>%
  mutate(across(p11:p22, ~as.numeric(.x)))

pairs <- pairs %>%
  mutate(contains = (p11>=p21 & p12 <=p22) | (p21>=p11 & p22 <=p12) )

sum(pairs$contains)

#part2
pairs %>%
  mutate(overlap = !(p11>p22 | p21>p12)) %>%
  summarise(overlap = sum(overlap))
