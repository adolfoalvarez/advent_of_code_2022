library(tidyverse)
buffer <- readLines("input6.txt")

#Classic for cycle
for (i in 1:(nchar(buffer)-3)){
  marker <- str_sub(buffer,i,i+3)
  if(max(str_count(marker, letters))==1) break
}
i+3

#part 2
for (i in 1:(nchar(buffer)-13)){
  marker <- str_sub(buffer,i,i+13)
  if(max(str_count(marker, letters))==1) break
}
i+13

