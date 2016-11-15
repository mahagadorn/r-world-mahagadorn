diamond.square.step<- function(g.mat){
  #Apply diamond sqaure steps to the inital matrix
  g.mat<-diamond.step(g.mat)
  g.mat<- square.step(g.mat)
  #we want to create the mid points for the sub-quadrants
  #preallocate the midpoint matrix
  Top.mid <- c(1, ceiling((1/4)*ncol(g.mat)), ceiling((1/2)*ncol(g.mat)), ceiling((3/4)*ncol(g.mat)), ncol(g.mat))
  Bot.mid <- c(1, ceiling((1/4)*nrow(g.mat)), ceiling((1/2)*nrow(g.mat)), ceiling((3/4)*nrow(g.mat)), nrow(g.mat))
  mid.points <- expand.grid(Bot.mid, Top.mid) #list of all our midpoints
  for(i in 1:nmid.points)){
      g.mat[1:3,1:3] <- diamond.step(g.mat[mid.points[i,]])
  }
  return(g.mat)
}
  
  
diamond.square.step(g.mat)
  
  
  
  
  
  
  return(g.mat)
}

g.mat<- diamond.square.step(g.mat)



Col.mid <- c(1, ceiling((1/2)*ncol(g.mat)), ncol(g.mat))
Row.mid <- c(1, ceiling((1/2)*nrow(g.mat)), nrow(g.mat))

MPBig <- expand.grid(Row.mid,Col.mid)







mid.matrix <- 
  
  
  Big Quads






#first segs
TLeft <- g.mat[1:mid.point, 1:mid.point]
TRight <- g.mat[1:mid.point, mid.point:full]
BLeft <- g.mat[mid.point:full, 1:mid.point]
BRight <- g.mat[mid.point:full, mid.point:full]
mid.point <- as.matrix(TLeft,TRight,BLeft,BRight)