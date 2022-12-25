#day 16
# Approach: I want today ALSO to try network analysis with igraph. Then we can find all paths...

#First to parse all necessary values
library(tidyverse)
library(igraph)
data <- readLines("input16.txt")
edges <- data %>% 
  str_remove("Valve ") %>% 
  str_replace(" has.*to valve[s]? ", "-") %>% 
  str_replace_all(",", ":") %>% 
  str_remove_all(" ")

values <- data %>% 
  str_extract("[0-9]+") %>% 
  as.numeric()

nodes <- edges %>% 
  str_remove("-.*")

nodes <- tibble(nodes, values)

valves <- nodes %>% 
  filter(values>0) %>% 
  pull(nodes)

# We make the graph and obtain the distances
graph_from_string <- function(x) {
  e <- str2expression(x)
  do.call(igraph:::graph_from_literal_i, list(e))
}
graph <- graph_from_string(edges)
d <- distances(graph)

#With distances now we can focus only on those valves that can be opened

newd <- d[c("AA",valves),c("AA",valves)]
allcosts <- newd %>% as_tibble() %>% 
  mutate(origin = c("AA", valves)) %>% 
  pivot_longer(-origin) %>% 
  rename(destination = name, time = value) %>% 
  filter(origin != destination) %>% 
  mutate(time = time + 1) %>% 
  filter(destination != "AA")

#And here we iterate, over the order of opening the valves

allpaths <- vector(mode = "list", length = length(valves))
prevstep <- allcosts %>% 
  filter(origin == "AA") %>% 
  select(node1 = origin, destination, time)

prevstep <- prevstep %>% 
  left_join(nodes, by = c("destination" = "nodes")) %>% 
  mutate(values = (30-time)*values)
allpaths[[1]] <- prevstep
for (i in 2:length(valves)){
  print(i)
  nextstep <- left_join(prevstep, allcosts, by = c("destination" = "origin")) %>% 
    mutate(time = time.x + time.y) %>% 
    select(-time.x, -time.y) %>% 
    filter(time<=30) %>% 
    select(-values, values)
  
  if(nrow(nextstep) == 0) break
  names(nextstep) <- c(paste0("node", 1:i), "destination", "time", "values")
  prevstep <- nextstep %>% mutate(path = 1:nrow(.))
  
  goodpaths <- prevstep %>% select(node1:destination, path) %>% pivot_longer(node1:destination, values_to = "time") %>% 
    group_by(path) %>% 
    summarise(flag = length(unique(time))) %>% 
    filter(flag == max(flag)) %>% 
    pull(path)
  prevstep <- prevstep %>% 
    filter(path %in% goodpaths) %>% 
    select(-path)
  
  prevstep <- prevstep %>% 
    left_join(nodes, by = c("destination" = "nodes")) %>% 
    mutate(values.y = (30-time)*values.y) %>% 
    mutate(values = values.x + values.y) %>% 
    select(-values.x, -values.y)
  
  allpaths[[i]] <- prevstep
}
results <- bind_rows(allpaths)
max(results$values)
#or
results %>% 
  filter(values == max(values))

#Part 2

