# Authors: Henrik Karlsson, Milda Poceviciute

#' @title Euclidean Algorithm
#' @name euclidean
#' @param a A number.
#' @param b A number.
#' @return A number, which is the greatest common divisor.
#' @description Euclid's algorithm is an efficient method for computing the greatest common divisor of two numbers. 
#' The largest number that divides both of them without leaving a remainder.
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}


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