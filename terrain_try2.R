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
  return(g.mat)
}

g.mat <- general.matrix(4) 


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
  g.mat<-diamond.step(g.mat)
  g.mat<- square.step(g.mat)
  return(g.mat)
}

g.mat<- diamond.square.step(g.mat)




#now using the diamond.square.step we want to fill in the rest of the tiny squares
















###Upper Left

up.left <- diamond.square.step(g.mat[1:(ceiling(.5*(nrow(g.mat)))), 1:ceiling(.5*(ncol(g.mat)))])
up.right <- diamond.square.step(g.mat[1:(ceiling(.5*(nrow(g.mat)))), ceiling(.5*(ncol(g.mat))):ncol(g.mat)])
#bind them together #####need to figure out how to not add an extra column in here
top.mat <- cbind(up.left, up.right)

bot.left <- diamond.square.step(g.mat[ceiling(.5*(nrow(g.mat))):nrow(g.mat), 1:ceiling(.5*(ncol(g.mat)))])
bot.right <- diamond.square.step(g.mat[ceiling(.5*(nrow(g.mat))):nrow(g.mat), ceiling(.5*(ncol(g.mat))):ncol(g.mat)])
#Bind them together
bot.mat <- cbind(bot.left, bot.right)

big.matrix <- rbind(top.mat, bot.mat)






