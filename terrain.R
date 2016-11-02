##MAHagadorn
##R-World
##Terrain
##November 2, 2016

#first thing we need to do is to make the square.matrix function

diamond.matrix <- function(x){
  matrix <- matrix(NA, nrow = ((2*x)+1), ncol = ((2*x)+1))
  matrix[1,1] <- rnorm(1)
  matrix[nrow(matrix), 1] <- rnorm(1)
  matrix[1, ncol(matrix)] <- rnorm(1)
  matrix[nrow(matrix), nrow(matrix)] <- rnorm(1)
  corner.vec <- c(matrix[1,1],  matrix[1, ncol(matrix)],  matrix[1, ncol(matrix)], matrix[nrow(matrix), nrow(matrix)])
  matrix[ceiling(.5*(nrow(matrix))), ceiling(.5*(ncol(matrix)))] <- mean(corner.vec)
  return(matrix)
} 

a <- diamond.matrix(x=5)

square.matrix <- function(x){
  matrix[ceiling(.5*nrow(matrix)),1] <- mean(c(matrix[1,1], matrix[nrow(matrix),1],  matrix[ceiling(.5*(nrow(matrix))), ceiling(.5*(ncol(matrix)))]))
  matrix[ceiling(.5*nrow(matrix)), ceiling(.5*ncol(matrix))] <- mean(c(matrix[1, ceiling(.5*ncol(matrix))],  matrix[nrow(matrix), nrow(matrix)],  matrix[ceiling(.5*(nrow(matrix))), ceiling(.5*(ncol(matrix)))]))
  return(matrix)
}

square.matrix(a)
