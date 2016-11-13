##MAHagadorn
##R-World
##Terrain
##November 2, 2016


#first thing that we need to do is to make a general matrix.
#matrix is called the g.mat, so we are saving these in specific places.
general.matrix <- function(n){
  mat.size <- ((2^n)+1)
  g.mat <- matrix(NA, nrow = mat.size, ncol = mat.size)
  #here we are adding values to the corners
  g.mat[1,1] <- rnorm(1, 500, 500)
  g.mat[nrow(g.mat), 1] <- rnorm(1, 500, 500)
  g.mat[1, ncol(g.mat)] <- rnorm(1, 500, 500)
  g.mat[nrow(g.mat), nrow(g.mat)] <- rnorm(1, 500, 500)
  return(g.mat)
}

g.mat <- general.matrix(3) 


#Diamond step matrix
diamond.step <- function(g.mat){
  corner.vec <- c(g.mat[1,1], g.mat[1, ncol(g.mat)],  g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), nrow(g.mat)])    #give me the values of the corners
  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))] <- mean(corner.vec)     #taking the mean of the corner and placing it in the center of the matrix
  return(g.mat)
}

ds <- diamond.step(g.mat)



#Square step matrix
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
  mid.points <- expand.grid(Bot.mid, Top.mid) #list of all our midpoints
  mid.points.mat <- matrix(unlist(mid.points), ncol=nrow(mid.points), byrow = TRUE)
  for(i in 1:nrow(mid.points.mat)){
    for(j in 1:nrow(mid.points.mat)){
    g.mat[1:i,1:j] <- diamond.step(g.mat[mid.points.mat[1:i,1:j]])
    }
  }
  return(g.mat)
}

g.mat<- diamond.square.step(g.mat)

