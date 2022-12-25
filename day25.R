library(tidyverse)
data <- read_lines("input25.txt")

converter <- function(x){
  x <- str_split_1(x, pattern = "")
  y <- ifelse(x=="=", -2, ifelse(x == "-",-1,x))
  y <- as.numeric(y)
  return(sum(y*5^((length(x)-1):0)))
}
results <- rep(0, length(data))
for (i in 1:length(results)){
  results[i] <- converter(data[i])
}
sum(results)
# base_converter <- function(x, base = 2){
#   remainder <- NULL
#   while(T){
#     remainder <- c(x%%base, remainder)
#     result <- x%/%base
#     x <- result
#     if(result==0) break
#   }
#   y <- paste0(remainder, collapse = "")
#   y <- as.numeric(y)
#   return(y)
# }

reverse_converter <- function(x, base = 5){
  remainder <- NULL
  while(T){
    remainder <- c(x%%base, remainder)
    test_remainder <- remainder[1]
    if(test_remainder == 4){
      result <- x%/%base +1
      remainder[1] <- -1
    }
    
    if( test_remainder == 3){
      result <- x%/%base +1
      remainder[1] <- -2
    }
    
    if(!test_remainder %in% c(3,4)){
      result <- x%/%base
    }
    x <- result
    if(result==0) break
  }
  y <- ifelse(remainder == -2, "=", remainder)
  y <- ifelse(y == -1, "-", y)
  y <- paste0(y, collapse = "")
  return(y)
}
reverse_converter(2022)
reverse_converter(sum(results))
