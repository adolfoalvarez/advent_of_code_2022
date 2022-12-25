library(tidyverse)
data <- read_delim("input11.txt", delim = ":", col_names = c("operation", "value"))

items <- data %>% 
  filter(grepl("Starting items", operation)) %>% 
  select(value) %>% 
  split(, f = 1:nrow(.))

items_numeric <- function(item){
  items <- str_extract_all(item, '[0-9]*', simplify = T) %>% as.numeric()
  items <- items[!is.na(items)]
  return(items)
}
items <- items %>% 
  map(items_numeric)

functions <- data %>% 
  filter(operation == "  Operation") %>% 
  select(value) %>% 
  pull() %>% 
  str_remove(" new = ") 

  
myfun <- function(x){
  return(function (old) return(floor(eval(str2expression(x))/3)))
}
functions <- functions %>% 
  map(myfun)

divisible <- data %>% 
  filter(operation == "  Test") %>% 
  select(value) %>% 
  pull() %>% 
  str_remove(" divisible by ") %>% 
  as.numeric()
names(items) <- paste0("Monkey ", 0:(length(items)-1))

iftrues <- data %>% 
  filter(operation == "    If true") %>% 
  select(value) %>% 
  pull() %>% 
  str_remove(" throw to monkey ") %>% 
  as.numeric()

iffalses <- data %>% 
  filter(operation == "    If false") %>% 
  select(value) %>% 
  pull() %>% 
  str_remove(" throw to monkey ") %>% 
  as.numeric()
tested_items <- rep(0, length(items)) %>% split(1:length(items))
for (j in 1:20){
  print(j)
for (i in 1:length(items)){
  z <- functions[[i]](items[[i]])
  division <- z%%divisible[i] == 0
  tested_items[[i]] <- tested_items[[i]] + length(division)
  ztrue <- z[division]
  zfalse <- z[!division]
  items[[i]] <- vector("numeric")
  items[[iftrues[i]+1]] <- c(items[[iftrues[i]+1]], ztrue)
  items[[iffalses[i]+1]] <- c(items[[iffalses[i]+1]], zfalse)
  
}
}

tested_items %>% unlist() %>% sort(decreasing = T) %>% `[`(1:2) %>% prod()

# Part 2

