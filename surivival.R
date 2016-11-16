#writing a functio for the plant.timestep method


#shouldn't we include an interations argument?...one that would specify time? maybe not

#I have no clue if this is even reasonable close to what we are supposed to be doing....AHHHHHHHH
plant.timestep <- function(plants, terrain, info){
  #define survivor function
  survive <- function(plant, info){
    if(is.na(plant))     #if it isnt a species I want you to return what is already in the contents of the cell
      return(NA)
    if (=='')
      return('')
    if(runif(1) <= info$survive[plant])   #your value is greater than or equal to your survival probability then you win yay!
      return(plant)
    if(runif(1) >= info$survive[plant])   #if the random number is greater that our survival probablity the return a blank space
      return('')    #this makes sense because if it dies it's no longer there...there is nothing in this cell
    }
    #looping through the plant matrix
    for(i in plants){
      for(j in terrain)
        new.plant.matrix <- survive(plants[i,j], info)
        return(new.plant.matrix)
  }
}








#Survival function FAIL
#this determines whether a particular species will survive
survive.fun <- function(cell, info){
  for(k in 1:(dim(plants)[3]-1)){
    for(i in 1:(dim(plants)[1])){
      for(j in 1:(dim(plants)[2])){
        if(is.na(cell)){     #if it isnt a species I want you to return what is already in the contents of the cell
          return(NA)
        }
        else{
          if(cell==''){
            return('')
          }
          else{
            if(runif(1) <= info$survive[cell]){ #your value is greater than or equal to your survival probability then you win yay!
              return(cell)
            }
            else{
              if(runif(1) >= info$survive[cell]){   #if the random number is greater that our survival probablity the return a blank space
                return('')    #this makes sense because if it dies it's no longer there...there is nothing in this cell
              }
            }
          }
        }
      }
    }
  }
}



#Survival function FAILED AGAIN!!!!!
#this determines whether a particular species will survive
?










#fail

run.plant.ecosystem <- function(terrain, num.timesteps, info){
  #Make the array
  plants <- array("", dim=c(dim(terrain), num.timesteps + 1))
  # for(i in 1:(.5*(nrow(terrain)*ncol(terrain))))   #######This doesn't work
  #below is how you RANDOMLY SEED YOUR PLANT MATRIX!!!!
  for(k in 1:(length(terrain))){
    plants[sample(nrow(plants),1), sample(ncol(plants),1), 1] <- sample(info$name, 1)
  }
  for(k in seq_len(dim(plants)[3])){
    #seq_len(y) or in our case (seq_len(dim(plants)) is creating a sequence up dimensions of plants array
    plants[,,k][is.na(terrain)] <- NA
  }
  for(i in 1:num.timesteps){
    for(j in 1:num.timesteps){
      for(k in 1:num.timesteps){
        plants[i,j,k] <- plant.timestep(plants, info)
      }
    }
  }
  return(plants)
}











## testing

survive.fun <- function(cell, info){
  if(is.na(cell))
    return(NA)
  if(cell=='')
    return('')
  if(runif(1) <= info$survive[cell])
    return(cell)
  if(runif(1) >= info$survive[cell])
    return('')   #this makes sense because if it dies it's no longer there...there is nothing in this cell
}


plant.timestep <- function(plants=plants, info=info){
  for(k in 1:(dim(plants)[3]-1)){
    for(i in 1:(dim(plants)[1])){
      for(j in 1:(dim(plants)[2])){
        plants[i,j,(k+1)] <- survive.fun(plants[i,j,k], info)
      }
    }
  }
  return(plants)
}


