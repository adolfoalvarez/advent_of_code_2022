library(tidyverse)
data <- readLines("input21.txt")
data <- str_replace(data, ":", " <-")
data <- paste0("try(", data, ")")
write_lines(data, file = "day21_prepared.R")

round <- 1
while(!exists("root")){
  source("day21_prepared.R")
}
sprintf("%.0f",root) 

#Part2
data <- readLines("input21.txt")
data <- str_replace(data, ":", " <-")
data[which(str_detect(data, "^humn"))]
data <- data[which(!str_detect(data, "^humn"))]
data[which(str_detect(data, "^root"))]
data <- c(data, "value <- fzvp-grlb")
data <- paste0("try(", data, ", silent = T)")
write_lines(data, file = "day21b_prepared.R")
rm(list=ls())

vfrom <- 1
vto <- 1e20
humn <- vfrom
while(!exists("value")){
  source("day21b_prepared.R")
}
value_from <- value

try(rm(list = setdiff(ls(), c("vfrom", "vto", "value_from"))))
humn <- vto
while(!exists("value")){
  source("day21b_prepared.R")
}
value_to <- value
try(rm(list = setdiff(ls(), c("vfrom", "vto", "value_from", "value_to"))))
for (j in 1:100){
  print(paste0("ITERATIONNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN ", j))
  middle <- (vfrom+vto)/2
  try(rm(list = setdiff(ls(), c("vfrom", "vto", "value_from", "value_to", "middle", "j"))))
  humn <- middle
    while(!exists("value")){
      source("day21b_prepared.R")
    }
  value_middle <- value
  if(sign(value_from) == sign(value_middle)){
    vfrom <- middle
  } else {
    vto <- middle
  }
  print(value_middle)
  if(round(vfrom,0) == round(vto,0)) break
  }
  
sprintf("%.0f",middle) 
  
