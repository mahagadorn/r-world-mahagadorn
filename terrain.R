##MAHagadorn
##R-World
##Terrain
##November 2, 2016


#first thing that we need to do is to make a general matrix.
#matrix is called the g.mat, so we are saving these in specific places.
general.matrix <- function(n){
  mat.size <- ((2*n)+1)
  g.mat <- matrix(NA, nrow = mat.size, ncol = mat.size)
  #here we are adding values to the corners
  g.mat[1,1] <- rnorm(1, 500, 500)
  g.mat[nrow(g.mat), 1] <- rnorm(1, 500, 500)
  g.mat[1, ncol(g.mat)] <- rnorm(1, 500, 500)
  g.mat[nrow(g.mat), nrow(g.mat)] <- rnorm(1, 500, 500)
  corner.vec <- c(g.mat[1,1], g.mat[1, ncol(g.mat)],  g.mat[1, ncol(g.mat)], g.mat[nrow(g.mat), nrow(g.mat)])    #give me the values of the corners
  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))] <- mean(corner.vec)     #taking the mean of the corner and placing it in the center of the matrix
  return(g.mat)
}

g.mat <- general.matrix(4) 


#diamond.step function
diamond.step <- function(g.mat){
  #now we need to add in to make the mini boxes
  #Start with the top left
  g.mat[ceiling(.5*nrow(g.mat)), 1] <- rnorm(1, 250, 100) #bottom left corner of top left mini-square
  g.mat[1, ceiling(.5*ncol(g.mat))] <- rnorm(1, 250, 100) #top right corner of the top left mini-square
  tl.mid <- c(g.mat[1,1], g.mat[1, ceiling(.5*ncol(g.mat))], g.mat[ceiling(.5*nrow(g.mat)), 1], g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))])
  #fill in the middle of the top corner
  g.mat[ceiling((1/4)*nrow(g.mat)), ceiling((1/4)*ncol(g.mat))] <- mean(tl.mid)
  #moving on to bottom left box
  #all points but the bottom right corner of the bottom left box are already filled. Going to generate that here
  g.mat[nrow(g.mat), ceiling(.5*ncol(g.mat))] <- rnorm(1, 250, 100)  #bottom right corner of bottom left box
  bl.mid <- c(g.mat[1, ceiling(.5*ncol(g.mat))], g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[nrow(g.mat), ncol(g.mat)], g.mat[nrow(g.mat), ceiling(.5*ncol(g.mat))])
  g.mat[ceiling((3/4)*nrow(g.mat)), ceiling((1/4)*ncol(g.mat))] <- mean(bl.mid)
  #working on the bottom right box
  g.mat[ceiling(.5*nrow(g.mat)), ncol(g.mat)] <- rnorm(1, 250, 100)
  br.mid <- c(g.mat[ceiling(.5*nrow(g.mat)), ncol(g.mat)], g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[nrow(g.mat), ceiling(.5*(ncol(g.mat)))], g.mat[nrow(g.mat), ncol(g.mat)])
  g.mat[ceiling((3/4)*nrow(g.mat)), ceiling((3/4)*ncol(g.mat))] <- mean(br.mid)
  #now just need to add the middle of the top right box
  tr.mid <- c(g.mat[ceiling(.5*nrow(g.mat)), ncol(g.mat)], g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))], g.mat[nrow(g.mat), ceiling(.5*(ncol(g.mat)))], g.mat[1, ncol(g.mat)])
  g.mat[ceiling((1/4)*nrow(g.mat)), ceiling((3/4)*ncol(g.mat))] <- mean(tr.mid)
  return(g.mat)
} 

diamond.step(g.mat)




ds#now we need to make our square matrix
square.step <- function(g.mat){
  g.mat[ceiling(.5*nrow(g.mat)),1] <- mean(c(g.mat[1,1], g.mat[nrow(g.mat),1],  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))]))
  # g.mat[ceiling(.5*nrow(g.mat)), ceiling(.5*ncol(g.mat))] <- mean(c(g.mat[1, ceiling(.5*ncol(g.mat))], g.mat[nrow(g.mat), nrow(g.mat)],  g.mat[ceiling(.5*(nrow(g.mat))), ceiling(.5*(ncol(g.mat)))]]
  return(g.mat)
}

square.step(g.mat)




















