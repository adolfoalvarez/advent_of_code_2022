#day 12
# Approach: I want today to try network analysis with igraph. Then we can find the shortest path.
library(tidyverse)
data <- readLines("input12.txt")
rows <- length(data)
data <- data %>% paste0(collapse = "")
data <- str_split(data, "", simplify = T) %>% as.vector()
data_matrix <- matrix(data, nrow = rows)
posx <- row(data_matrix)
posy <- col(data_matrix)

data2 <- tibble(data, posx = as.numeric(t(posx)), posy= as.numeric(t(posy)), number = 1:length(data))


starting_point <- data2 %>% 
  filter(data == "S")

end_point <- data2 %>% 
  filter(data == "E")

data2 <- data2 %>% 
  mutate(data = ifelse(data =="S", "a", data),
         data = ifelse(data == "E", "z", data))

data2 <- data2 %>% 
  left_join(tibble(data = letters, value = 1:length(letters))) 

adj_data <- matrix(F, nrow = length(data), ncol = length(data))


#I hate to do this:
number_cols <- ncol(posx)
for (i in 1:nrow(adj_data)){
  print(i)
  # for (j in 1:ncol(adj_data)){
  for (j in intersect(c(i-number_cols, i+number_cols, i-1,i+1),1:nrow(adj_data))){
    byrows <- abs(data2$posx[i]-data2$posx[j]) 
    bycols <- abs(data2$posy[i]-data2$posy[j]) 
    cond1 <-  byrows <= 1
    cond2 <-  bycols <= 1
    cond3 <- byrows == 0 | bycols == 0
    cond4 <- data2$value[j]<=data2$value[i]+1
    adj_data[i,j] <- cond1 & cond2 & cond3 & cond4
  }
}

#Y ahora igraph!
library(igraph)
adj_data <- 1*adj_data
g1 <- graph_from_adjacency_matrix(adj_data)
sp <- shortest_paths(g1, from = starting_point$number, to = end_point$number)
length(sp$vpath[[1]])-1

#Part2
a <- data2 %>% 
  filter(data == "a") %>% 
  pull(number)

allchoices <- rep(0, length(a))
for (i in 1:length(a)){
  print(i)
  sp <- shortest_paths(g1, from = a[i], to = end_point$number)
  allchoices[i] <- length(sp$vpath[[1]])-1
}
min(allchoices[allchoices>0])

