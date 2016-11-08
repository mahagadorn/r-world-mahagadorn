##MAHagadorn
##R-World
##Plants
##November 7, 2016

#The first thing that we need to do is write a defensive function
#this function will check a variety of things to ensure that all the input information is right
    ##basically we are checking to make sure the user isn't putting in any information that is wrong


#need to create our input vectors
#reproduction probability vector

#probability of reproduction for 3 different species
repro <- c(.50, 1, .75)

#survival vector containing probability of survival for 3 different species
survive <- c(.75, .50, .50)

#here is our competition matrix
comp.mat <- matrix(NA, nrow = length(repro), ncol=length(repro))
comp.mat[1,] <- c(1, .50, .25)
comp.mat[2,] <- c(.25, .35, .85)
comp.mat[3,] <- c(.75, .25, .90)
comp.mat

#names of our plant species
names <- c("A", "B", "C")


#here is the function that will set up our plants
setup.plants <- function(repro, survive, comp.mat, names=NULL){
  if (is.null(names))
    names <- letters[seq_along(repro)]
  if (length(repro) !=length(survive))
    stop("Reproduction and survival parameters needed for all species!")
  if (length(names) != length(repro))
    stop("The number of names doesn't match reproduction and survival parameters")
  repro <- setNames(repro, names)
  survive <- setNames(survive, names)
  #set.Names is a convenience function that sets the names on an object and returns the object
  #set.Names() is the most useful at the end of a function definition where one is creating the object to be returned
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}

info <- setup.plants(repro, survive, setup.plants, names)

#just testing to make sure that the added names section works out.
# test.name<-c("A", "B")
# setup.plants(repro, survive, setup.plants, test.name)

#First Steps: storing plants and keeping them alive!!

#Survival function
#this determines whether a particular species will survive
survive <- function(cell, info){
  if(is.na(cell))     #if it isnt a species I want you to return what is already in the contents of the cell
    return(NA)
  if (cell=='')
    return('')
  if(runif(1) <= info$survive[plant])   #your value is greater than or equal to your survival probability then you win yay!
    return(cell)
  if(runif(1) >= info$survive[plant])   #if the random number is greater that our survival probablity the return a blank space
    return('')    #this makes sense because if it dies it's no longer there...there is nothing in this cell
}


#pseudo code for what to feed into this function:
#matrix.name[row,col] <- survive(matrix[row,col], info).....this will ultimately be looped over!!!


#now we need a function that makes time "tick" by for an entire matrix of plants
#this function should have an input that is our plant matrix
#we want to loop over the plant matrix
#then we want to apply the survivor function to all of the contents

#what we want is a loop for the plant matrix and then a loop for the survivor matrix



#plant.timestep function

#I have no clue if this is even reasonably close to what we are supposed to be doing....AHHHHHHHH
plant.timestep <- function(plants, terrain, info){
  #define survivor function
  survive <- function(plant, info){
    if(is.na(plant))     #if it isnt a species I want you to return what is already in the contents of the cell
      return(NA)
    if (plant=='')
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



#6.2.3 run.plant.ecosystem---Here is where we actually make our plant array that we input into the previously generated functions
#info was generated withthe setup.plants

#what do we want here???








