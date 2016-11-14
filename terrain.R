##'Generation of a terrain matrix that will be fed into plant simulation
##MAHagadorn
##Terrain
##November 2, 2016

#'Initial commenting.
#'Make a terrain matrix of numeric values using the Diamond-Square Step Algorithm
#'
#'@param n Size of the grid will be a (2^n)+1 (by default: n=3; a 9 x 9 grid)
#'
#'@param noise; numeric vector of length 2. This vector is used to generate random noise added at each step of the diamond.square algorithm.
#'    The input vector is ultimately split into two single numeric elements called SD1 and SD2.
#'    The first value (SD1) in the vector will be used as the standard deviation in the generation of the first diamond.square step.  
#'    The second value (SD2) will be used as the standard deviation will be used for noise at the finer spatial scales.
#'        
#'
#'@param need to add more onces you get your terrain working....
#'
#'Functions used in this script include
#'    general.matrix
#'    diamond.step
#'    square.step
#'    diamond.square.step





#'general.matrix; this function preallocates the matrix based on size n.
#'Specifically, the four corners are generated.
#'
#'@param n; Size of the grid will be a (2^n)+1 (by default: n=3; a 9 x 9 grid)
#'
#'@param mean; numeric value that will be input as the mean into the rnorm() function.  
#'    This function is used to generate the random corner values from a normal distribution.
#'    The default for this value is 500.
#'
#'@param SD1; numeric value that will be input as the standard deviation into the rnorm() function. 
#'    This function is used to generate the random corner values from a normal distribution.
#'    
#'    The default for this value is 500.

#' Starting mean value
mean <- 500

#' Random noise. See discription above
noise <- c(500, 100)
SD1 <- noise[1]
SD2 <- noise[2]

general.matrix <- function(n=3, mean=500, SD1=500){
  if(is.null(n)){
    n <- 3
  }
  mat.size <- ((2^n)+1)
  g.mat <- matrix(NA, nrow = mat.size, ncol = mat.size)
  #' here we are adding values to the corners
  g.mat[1,1] <- rnorm(1, mean, SD1)
  g.mat[nrow(g.mat), 1] <- rnorm(1, mean, SD1)
  g.mat[1, ncol(g.mat)] <- rnorm(1, mean, SD1)
  g.mat[nrow(g.mat), nrow(g.mat)] <- rnorm(1, mean, SD1)
  return(g.mat)
}

g.mat <- general.matrix(n, mean, SD1) 


#' Diamond step matrix
#' Takes mean of the randomly generated corner values to make the the center point
diamond.step <- function(g.mat){
  corner.vec <- c(g.mat[1,1], g.mat[1, ncol(g.mat)],  g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), nrow(g.mat)])    #give me the values of the corners
  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))] <- mean(corner.vec)     #taking the mean of the corner and placing it in the center of the matrix
  return(g.mat)
}

ds <- diamond.step(g.mat)



#' Square step matrix
#' MAH ADD STUFF HERE
square.step <- function(g.mat){
  #need to make the left middle point
  corner.vec <- c(g.mat[1,1], g.mat[1, ncol(g.mat)],  g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), nrow(g.mat)])    #give me the values of the corners
  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))] <- mean(corner.vec) 
  left.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[nrow(g.mat), 1])
  top.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[1, ncol(g.mat)])
  right.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), ncol(g.mat)])
  b.mid <- c(g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[1,1], g.mat[nrow(g.mat), ncol(g.mat)])
  #assigning means to spot in matrix
  g.mat[ceiling(.5*(nrow(g.mat))), 1] <- mean(left.mid)
  g.mat[1, ceiling(.5*(ncol(g.mat)))] <- mean(top.mid)
  g.mat[ceiling(.5*(nrow(g.mat))), nrow(g.mat)] <- mean(right.mid)
  g.mat[nrow(g.mat), ceiling(.5*(ncol(g.mat)))] <- mean(b.mid)
  return(g.mat) 
}

ss <- square.step(g.mat)



#writing a diamond step function that does both the functions above into one wrapper function????
diamond.square.step<- function(g.mat){
  #Apply diamond sqaure steps to the inital matrix
  g.mat<-diamond.step(g.mat)
  g.mat<- square.step(g.mat)
  #we want to create the mid points for the sub-quadrants
  #preallocate the midpoint matrix
  Top.mid <- c(1, ceiling((1/4)*ncol(g.mat)), ceiling((1/2)*ncol(g.mat)), ceiling((3/4)*ncol(g.mat)), ncol(g.mat))
  Bot.mid <- c(1, ceiling((1/4)*nrow(g.mat)), ceiling((1/2)*nrow(g.mat)), ceiling((3/4)*nrow(g.mat)), nrow(g.mat))
  mid.points <- as.matrix(expand.grid(Bot.mid, Top.mid)) #list of all our midpoints
  for(i in 1:nrow(mid.points)){
    for(j in 1:ncol(mid.points.mat)){
    g.mat[1:i,1:j] <- diamond.step(g.mat[mid.points[1:i,1:j]])
    }
  }
  return(g.mat)
}

g.mat<- diamond.square.step(g.mat)

