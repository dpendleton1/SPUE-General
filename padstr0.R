# padstr0 <- function(x,n){
#   #pad zeros to the left of numbers
#   #x is your number
#   #n is the length of padding
#   library(stringr)
#   x = toString(x)
#   while (str_length(x) < n){
#     x = paste0("0", x)
#   }
#   return(x)
# }

padstr0 <- function(x,n){
  #pad zeros to the left of numbers
  #x is your number
  #n is the length of padding
  library(stringr)
  y = NA
  for (i in 1:length(x)){
    xx = toString(x[i])
    while (str_length(xx) < n){
      xx = paste0("0", xx)
    }
    y[i] = xx
  }
  return(y)
}

# a = dat$GMT[1000:1010]
# 
# library(stringr)
# x = toString(x)
# while (str_length(x) < n){
#   x = paste0("0", x)
# }
# return(x)
