library(tidyverse)
steps <- read_delim("input9.txt", delim = " ", col_names = FALSE)
#total steps
total_steps <- steps$X2 %>% sum()
#How far away we are?
steps %>% 
  group_by(X1) %>% 
  summarise(X2 = sum(X2))
#since max number of steps in any direcion is <3000, a 6001 by 6001 square is big enough
library(tidyr)
steps <- uncount(steps, X2) %>% pull()
#There are 11322 steps, in a 6001x6001 square
#For each step I need the new pos of H
#Then, a function of the new pos of T in base of new pos of H and old pos of T
#And for each pos of T we change the value of the matrix from 0 to 1 (or F to T)

field <- matrix(rep(0, 6001*6001), nrow = 6001)
pos_H <- c(3001, 3001)
pos_T <- c(3001, 3001)
field[3001,3001] <- 1
#Let's find the final position of H

myround <- function(x){
  z <- x/2
  z <- ifelse(z<0,floor(z),ceiling(z))
  return(z)
}
change_T <- function(pos_H, pos_T){
  dif_HT <- pos_H-pos_T
  if(max(abs(dif_HT))==2){
    pos_T <- pos_T + myround(dif_HT)
  }
  return(pos_T)
}


for (i in 1:total_steps){
  print(i)
  pos_H <- case_when(
    steps[i] == "L" ~ pos_H + c(0,-1),
    steps[i] == "R" ~ pos_H + c(0,1),
    steps[i] == "U" ~ pos_H + c(-1,0),
    steps[i] == "D" ~ pos_H + c(1,0),
  )
  pos_T <- change_T(pos_H, pos_T)
  field[pos_T[1], pos_T[2]] <- 1
}
sum(field)

#Part 2

field <- matrix(rep(0, 6001*6001), nrow = 6001)
rope <- vector(mode = "list", length = 10L)
rope <- rope %>% 
  map(function(x) x=c(3001, 3001))

for (i in 1:total_steps){
  print(i)
    rope[[1]] <- case_when(
      steps[i] == "L" ~ rope[[1]] + c(0,-1),
      steps[i] == "R" ~ rope[[1]] + c(0,1),
      steps[i] == "U" ~ rope[[1]] + c(-1,0),
      steps[i] == "D" ~ rope[[1]] + c(1,0),
    )
    for (j in 2:10){
      rope[[j]] <- change_T(rope[[j-1]], rope[[j]])
    }
  field[rope[[10]][1], rope[[10]][2]] <- 1
}
sum(field)
