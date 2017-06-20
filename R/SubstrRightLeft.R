# function to retain the n chars on the right side
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# function to retain all chars but the n chars on the right side
substrLeft <- function(x, n){
  substr(x, 0, nchar(x)-n)
}