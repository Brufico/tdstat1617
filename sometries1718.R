
nonavect <- function(vect) {vect[which(!is.na(vect))]}


# generate random

makexnorm <- function(n = 30,  m = 10, s = 3, numna = 3){
        x <- rnorm(n = n, mean = m, sd = s)
        x[x<0] <- 0
        x[round(runif(n = numna, min = 0, max = 30), 0)] <- repeat(NA, numna)
        x
}

set.seed(123)
xx <- makexnorm()
xx
