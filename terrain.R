##'Generation of a terrain matrix that will be fed into plant simulation
##MAHagadorn
##Terrain
##November 2, 2016


# Make a terrain matrix of numeric values using the Diamond-Square Step Algorithm
#
# Functions used in this script include
    # general.matrix; makes initial matrix
    # diamond.step; creates diamond step function
    # square.step; creates square step function
    # diamond.square.step; combines diamond and square step functions
    # make.terrain; wrapper function that includes diamond.square.step function and lakes.na (argument)
        #This is the only function that is exported!!!


#matrix size
# mat.size <- ((2^n)+1); will yield a matrix that will always have a center point/midpoints that can be calculated
general.matrix <- function(n, mean){
  if(is.null(n)){
    n <- 3
  }
  mat.size <- ((2^n)+1)
  g.mat <- matrix(NA, nrow = mat.size, ncol = mat.size)
  # here we are adding values to the corners
  # rnorm() adds a large amount of noise/variation to numeric outputs
  g.mat[1,1] <- rnorm(1, mean, (mean+mean))
  g.mat[nrow(g.mat), 1] <- rnorm(1, mean, (mean+mean))
  g.mat[1, ncol(g.mat)] <- rnorm(1, mean, (mean+mean))
  g.mat[nrow(g.mat), nrow(g.mat)] <- rnorm(1, mean, (mean+mean))
  return(g.mat)
}


# Diamond step matrix
# Takes mean of the randomly generated corner values to make the the center point
diamond.step <- function(g.mat){
  corner.vec <- c(g.mat[1,1], g.mat[1, ncol(g.mat)],  g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), nrow(g.mat)])    #give me the values of the corners
  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))] <- mean(corner.vec)     #taking the mean of the corner and placing it in the center of the matrix
  return(g.mat)
}

ds <- diamond.step(g.mat)



# Square step matrix
# This calculates the square step aspect of the terrain matrix
# first we create vecotrs which concatenate the values that need to be input for calculating the means
square.step <- function(g.mat){
  #need to make the left middle point
  corner.vec <- c(g.mat[1,1], g.mat[1, ncol(g.mat)],  g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), nrow(g.mat)])    #give me the values of the corners
  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))] <- mean(corner.vec)
  left.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[nrow(g.mat), 1])
  top.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[1, ncol(g.mat)])
  right.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), ncol(g.mat)])
  b.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[nrow(g.mat), ncol(g.mat)])
  #assigning means to spot in matrix; the rnorm() adds a decreased level of noise in this step
  g.mat[ceiling(.5*(nrow(g.mat))), 1] <- mean(left.mid) + rnorm(1, mean, (mean/2))
  g.mat[1, ceiling(.5*(ncol(g.mat)))] <- mean(top.mid) + rnorm(1, mean, (mean/2))
  g.mat[ceiling(.5*(nrow(g.mat))), nrow(g.mat)] <- mean(right.mid) + rnorm(1, mean, (mean/2))
  g.mat[nrow(g.mat), ceiling(.5*(ncol(g.mat)))] <- mean(b.mid) + rnorm(1, mean, (mean/2))
  return(g.mat)
}




#Writing a diamond step function that does both the functions above and fills in all values of our matrix
#This is flexible enough to do large and small n values
diamond.square.step<- function(g.mat, mean, n){
  mean <- mean
  n <- n
  for(i in 2^(n:1)){
    for(j in seq(1, ncol(g.mat)-1, by=i)){
      for(k in seq(1, nrow(g.mat)-1, by=i)){
        g.mat[k:(k+i),j:(j+i)] <- diamond.step(g.mat[k:(k+i),j:(j+i)])
        g.mat[k:(k+i),j:(j+i)] <- square.step(g.mat[k:(k+i),j:(j+i)], mean)
      }
    }
  }
  return(g.mat)
}


#' Make a terrain matrix of numeric values using the Diamond-Square Step Algorithm
#'
#' A light wrapper function around \code{diamond.square.step} and the addition of lakes to the terrain matrix
#' @param n Size of the grid will be a (2^n)+1 (by default: n=5; a 33 by 33 grid)
#' @param mean Numeric vector (Default: 500) that will be input as the mean into the rnorm() function.
#'    Standard deviations used in the rnorm() function are generated from the mean value.
#'    The first standard deviation value (calculated as mean * mean) will be used in \code{general.matrix} to generate  the random corner values from a normal distribution.
#'    The second standard deviation value (calculated as mean/2) will be used in \code{square.step} for generating noise at the finer spatial scales.
#' @param lake.na Logical (Default: TRUE) that specifies whether to make all terrain values lower than zero height underwater.
#' @return a terrain matrix; numeric elements that are indicates as heights and if lake.na=TRUE \code{NA}s indicated cells that are waterlogged. Terrain matrix is visualized using \code{image}.
#' @examples
#' x <- make.terrain(n=3, mean=100, lake.na = TRUE)
#' y <- make.terrain(lake.na = FALSE)
#' z <- make.terrain(2, 10)
#' @export


make.terrain <- function(n=5, mean=500, lake.na=TRUE){
  n <- n
  mean <- mean
  mat.size <- ((2^n)+1)
  g.mat <- general.matrix(n, mean)
  terrain <- diamond.square.step(g.mat, mean, n)
  #now we want to make the option to make anything less than zero NA or (H20)
  if(lake.na==TRUE){
    terrain[terrain < 0] <- NA
  }
  image(terrain, col = terrain.colors(20))
  return(terrain)
}

x <- make.terrain(n=3, mean=100, lake.na = TRUE)
y <- make.terrain(lake.na = FALSE)
z <- make.terrain(2, 10)




