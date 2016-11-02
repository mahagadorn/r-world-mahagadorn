##MAHagadorn
##R-World
##Terrain
##November 2, 2016

#first thing we need to do is to make the square.matrix function

square.matrix <- function(x){
  matrix <- matrix(NA, nrow = ((2*x)+1), ncol = ((2*x)+1))
  matrix[1,1] <- rnorm(1)
  matrix["nrow (x)", 1] <- rnorm(1)
  matrix[1, ncol(x)] <- rnorm(1)
  matrix[nrow(x), ncol(x)] <- rnorm(1)
  return(matrix)
} 

square.matrix(x=5)
