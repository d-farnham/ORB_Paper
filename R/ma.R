ma <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=1)}
