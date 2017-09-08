# Authors: Henrik Karlsson, Milda Poceviciute

# 1.1 Write the R code
# 1.1.1 euclidean()

euclidean <- function(a, b) {
  stopifnot(is.numeric(a), is.numeric(b))
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
    return(a)
    
  }
}