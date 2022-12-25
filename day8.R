library(Matrix)
library(tidyverse)
a <- readLines("input8.txt")
a <- split(a, 1:length(a))
a <- a %>% 
  map(str_split_1, "") %>% 
  map(as.numeric) %>% 
  bind_rows() %>% 
  as.matrix() %>% 
  t()

#I am not proud of this. It needs to be a better way.
visible_trees <- function(b){
  arriba <- matrix(rep(T,nrow(b)*ncol(b)), nrow = nrow(b))
  abajo <- arriba
  izquierda <- arriba
  derecha <- arriba
  for (i in 2:(nrow(b)-1)){
    for (j in 2:(ncol(b)-1)){
      arriba[i,j] <- all(b[i,j]>b[1:(i-1),j])
      abajo[i,j] <- all(b[i,j]>b[(i+1):nrow(b),j])
      izquierda[i,j] <- all(b[i,j]>b[i, 1:(j-1)])
      derecha[i,j] <- all(b[i,j]>b[i,(j+1):ncol(b)])
    }
  }
  return(sum(arriba | abajo | izquierda | derecha))
}
visible_trees(a)

#Part 2
library(BBmisc)

mywf <- function(x){
    z <- which.first(x)
    if(length(z) == 0) {
      z <- length(x)
    } 
    return(z)
}
scenic_score <- function(b){
  arriba <- matrix(rep(0,nrow(b)*ncol(b)), nrow = nrow(b))
  abajo <- arriba
  izquierda <- arriba
  derecha <- arriba
  for (i in 2:(nrow(b)-1)){
    for (j in 2:(ncol(b)-1)){
      arriba[i,j] <- mywf(rev(b[1:(i-1),j])>=b[i,j])
      abajo[i,j] <- mywf(b[(i+1):nrow(b),j]>=b[i,j])
      derecha[i,j] <- mywf(b[i,(j+1):ncol(b)]>=b[i,j])
      izquierda[i,j] <- mywf(rev(b[i, 1:(j-1)])>=b[i,j])
    }
  }
  return(arriba * abajo * izquierda * derecha)
}
max(scenic_score(a))
