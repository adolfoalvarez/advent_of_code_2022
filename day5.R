library(tidyverse)
library(stringi)
crates <- readLines("input5.txt", n = 8)
pos <- split(seq(2,8*4+2, by = 4), 1:9)
stack <- map(pos, \(pos) str_sub(x,pos,pos)) %>%
  map(str_flatten) %>%
  map(str_squish)

moves <- read_delim("input5.txt", delim = " ", col_names = FALSE, skip = 10) %>%
  select(X2, X4, X6) %>%
  rename(move = X2,
         from = X4,
         to = X6)

cran <- function(stack, move, from, to){
  stack[[to]] <- stack[[from]] %>%
    str_sub(1,move) %>%
    stri_reverse() %>%
    str_c(stack[[to]])

  stack[[from]] <- stack[[from]] %>%
    str_sub(move+1, nchar(.))
  return(stack)
}

for (i in 1:nrow(moves)){
  print(i)
  stack <- cran(stack, move = moves$move[i], from = moves$from[i], to = moves$to[i])
}
str_sub(stack,1,1) %>%
  str_flatten

#Part 2
stack <- map(pos, \(pos) str_sub(x,pos,pos)) %>%
  map(str_flatten) %>%
  map(str_squish)

cran9001 <- function(stack, move, from, to){
  stack[[to]] <- stack[[from]] %>%
    str_sub(1,move) %>%
    #stri_reverse() %>%
    str_c(stack[[to]])

  stack[[from]] <- stack[[from]] %>%
    str_sub(move+1, nchar(.))
  return(stack)
}

for (i in 1:nrow(moves)){
  print(i)
  stack <- cran9001(stack, move = moves$move[i], from = moves$from[i], to = moves$to[i])
}
str_sub(stack,1,1) %>%
  str_flatten
