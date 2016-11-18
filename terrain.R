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



#' Make starting terrain matrix with the corners seeded
#'
#' Function to make the initial terrain matrix with seeded corners. Ultimately, this matrix will be input into both the \code{diamond.step} and \code{square.step} functions.
#' @param n Size of the grid will be a (2^n)+1 (by default: n=5; a 33 by 33 grid)
#' @param x.bar Numeric vector (Default: 500) that will be input as the mean into the rnorm() function.
#'    Here, x.bar will be used to add random noise to our terrain matrix through standard deviations from the x.bar value.
#'    This is applied to rnorm() as sd=(x.bar*x.bar), so one half the x.bar will be the standard deviation used.
#' @author Mallory Hagadorn
#' @return general matrix called g.mat


general.matrix <- function(n=5, x.bar=500){
  mat.size <- ((2^n)+1)
  g.mat <- matrix(NA, nrow = mat.size, ncol = mat.size)
  # here we are adding values to the corners
  # rnorm() adds a large amount of noise/variation to numeric outputs
  g.mat[1,1] <- rnorm(1, x.bar, (x.bar*10))
  g.mat[nrow(g.mat), 1] <- rnorm(1, x.bar, (x.bar*10))
  g.mat[1, ncol(g.mat)] <- rnorm(1, x.bar, (x.bar*10))
  g.mat[nrow(g.mat), nrow(g.mat)] <- rnorm(1, x.bar, (x.bar*10))
  return(g.mat)
}
#MAH NOTE




#' Apply diamond step algorithm to a general matrix

#'
#' Adding values to a general matrix (g.mat) in a diamond algorithm fashion
#' @param g.mat General matrix generate using the \code{general.matrix} function (Default: g.mat).
#' @author Mallory Hagadorn
#' @return a general matrix (g.mat)


# Diamond step matrix
# Takes mean of the randomly generated corner values to make the the center point
diamond.step <- function(g.mat=g.mat){
  corner.vec <- c(g.mat[1,1], g.mat[1, ncol(g.mat)],  g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), nrow(g.mat)])    #give me the values of the corners
  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))] <- mean(corner.vec)     #taking the mean of the corner and placing it in the center of the matrix
  return(g.mat)
}




#' Apply square step algorithm to a general matrix
#'
#' Adding values to a general matrix (g.mat) in a square algorithm fashion
#' @param g.mat General matrix generate using the \code{general.matrix} function (Default: g.mat).
#' @param x.bar Numeric vector (Default: 500) that will be input as the mean into the rnorm() function.
#'    Here, x.bar will be used to add random noise to our terrain matrix through standard deviations from the x.bar value.
#'    This is applied to rnorm() as sd=(x.bar/2), so one half the x.bar will be the standard deviation used.
#' @author Mallory Hagadorn
#' @return a general matrix (g.mat)


# Square step matrix
# This calculates the square step aspect of the terrain matrix
# first we create vecotrs which concatenate the values that need to be input for calculating the means
square.step <- function(g.mat=g.mat, x.bar=500){
  #need to make the left middle point
  corner.vec <- c(g.mat[1,1], g.mat[1, ncol(g.mat)],  g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), nrow(g.mat)])    #give me the values of the corners
  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))] <- mean(corner.vec)
  left.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[nrow(g.mat), 1])
  top.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[1, ncol(g.mat)])
  right.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), ncol(g.mat)])
  b.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[nrow(g.mat), ncol(g.mat)])
  #assigning means to spot in matrix; the rnorm() adds a decreased level of noise in this step
  g.mat[ceiling(.5*(nrow(g.mat))), 1] <- mean(left.mid) + rnorm(1, x.bar, sd=(x.bar/2))
  g.mat[1, ceiling(.5*(ncol(g.mat)))] <- mean(top.mid) + rnorm(1, x.bar, sd=(x.bar/2))
  g.mat[ceiling(.5*(nrow(g.mat))), nrow(g.mat)] <- mean(right.mid) + rnorm(1, x.bar, sd=(x.bar/2))
  g.mat[nrow(g.mat), ceiling(.5*(ncol(g.mat)))] <- mean(b.mid) + rnorm(1, x.bar, sd=(x.bar/2))
  return(g.mat)
}


#' Apply the diamond and square step algorithms to a general matrix
#'
#' Adding values to a general matrix (g.mat) sequentially based on the \code{diamond.step} and \code{square.step} algorithms
#' @param g.mat General matrix generate using the \code{general.matrix} function (Default: g.mat).
#' @param n Size of the grid will be a (2^n)+1 (by default: n=5; a 33 by 33 grid)
#' @param x.bar Numeric vector (Default: 500) that will be input as the mean into the rnorm() function.
#'    This value is used to add random noise to the matrix values when functions \code{general.matrix} and \code{square.step} are called.
#' @author Mallory Hagadorn
#' @return a general matrix (g.mat)


#Writing a diamond step function that does both the functions above and fills in all values of our matrix
#This is flexible enough to do large and small n values
diamond.square.step<- function(g.mat=g.mat, n=5, x.bar=500){
  x.bar <- x.bar
  n <- n
  for(i in 2^(n:1)){
    #Will, I hope this is detailed enough to explain to you that I understand what is happening, I'm not the best at explaining in words.
    #If not by all means ask me.
    #Got inspiration from Bodie's work:
    #The above line is how we are able to make the subsets within our matrices
    #MAH Example for understanding, so if my default n is used (which is 5) we get this:
    # > 2^(5:1)
    # [1] 32 16  8  4  2
    #Therefore, we are getting a subet of all of our "midpoints" persay
    #This is what Will was trying to get you to see earlier...This will mean that your functions will loop over those matrice subsets.
    for(j in seq(1, (ncol(g.mat)-1), by=i)){
      #means: for (j) or our column index we want to take the j-th value that is in a sequence from 1 to the number of columns in our matrix (minus one)
      #we subtract one from this to keep it within the bounds "Without error message is  Error in g.mat[k:(k + i), j:(j + i)] : subscript out of bounds"
      #This error means that that index (here j) is out of the given bounds aka the specified size of our matrix
      #By making it ncol(g.mat)-1), so the number of actually columns in our matrix MINUS one means that we keep our script in bounds(or the given matrix size) and R is happy!
      #the by=i argument specifies that we want to sequence generated to be in increments that are equivalent to our matrix subsets.
      #by doing this in j we are saying to use this sequence for all of our indexed rows.
      for(k in seq(1, (nrow(g.mat)-1), by=i)){
        #See description above, except this is for our rows not our columns
        g.mat[k:(k+i),j:(j+i)] <- diamond.step(g.mat[k:(k+i),j:(j+i)])
        #Here we are just applying our previously defined diamond.step function to our row and column subsets.
        #This will loop through all our our large quadrants and then loop through our smaller quadrants.
        #NOTE MAH: it's important to remember that these loops are nested within each other
        #This starts with the first for loop [for(i in 2^(n:1))] where we make our initial subsets.
        g.mat[k:(k+i),j:(j+i)] <- square.step(g.mat[k:(k+i),j:(j+i)], x.bar)
        #same as above just applying out square.step function
      }
    }
  }
  return(g.mat)
}

#' Make a terrain matrix of numeric values using the Diamond-Square Step Algorithm
#'
#' A light wrapper function around \code{diamond.square.step} and the addition of lakes to the terrain matrix
#' @param n Size of the grid will be a (2^n)+1 (by default: n=5; a 33 by 33 grid)
#' @param x.bar Numeric vector (Default: 500) that will be input as the mean into the rnorm() function.
#'    Standard deviations used in the rnorm() function are generated from the mean value.
#'    The first standard deviation value (calculated as mean * mean) will be used in \code{general.matrix} to generate  the random corner values from a normal distribution.
#'    The second standard deviation value (calculated as mean/2) will be used in \code{square.step} for generating noise at the finer spatial scales.
#' @param water Logical (Default: TRUE) that specifies whether to make all terrain values lower than zero height underwater.
#' @return a terrain matrix; numeric elements that are indicates as heights and if lake.na=TRUE \code{NA}s indicated cells that are waterlogged. Terrain matrix is visualized using \code{image}.
#' @author Mallory Hagadorn
#' @examples
#' terrain <- make.terrain(3, 100, water = TRUE)
#'    #Returned is a matrix (and image) using 3 as the size indicator, 100 as the mean, and saying that any value less than or equal to 0 should be made an NA.
#' y <- make.terrain(water = FALSE)
#'    #Returned is a matrix (and image) which using the default n and x.bar values, but that specifies values less than or equal to 0 shouldn't be replaced with NA's
#'    #This gives the user some flexibility to not force them to use NA's
#' z <- make.terrain(2, 10)
#'    #Returned is a matrix (and image) where the n(2) and x.bar(10) are much smaller, but NA's still replace the values less than or equal to 0
#' @export


make.terrain <- function(n=5, x.bar=500, water=TRUE){
  n <- n
  x.bar <- x.bar
  mat.size <- ((2^n)+1)
  g.mat <- general.matrix(n, x.bar)
  terrain <- diamond.square.step(g.mat, n, x.bar)
  #now we want to make the option to make anything less than zero NA or (H20)
  if(water==TRUE){
    terrain[terrain <= 0] <- NA
  }
  image(terrain, col = terrain.colors(20))
  return(terrain)
}


