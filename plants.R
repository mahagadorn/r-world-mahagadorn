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
name <- c("M. sativa", "L. perenne", "T. repens")


#Here we are going to be using the Test.Terrain that I made in a seperate script (Test.Terrain.R)
#This will ensure that our functions are working properly
Test.Terrain
# [,1]  [,2]  [,3]   [,4]   [,5]
# [1,]  0.520 0.374 0.951  0.955  1.526
# [2,]  4.056 4.908 3.293  2.732  4.758
# [3,] -0.187 3.506    NA  2.269  2.224
# [4,]  1.011 0.761    NA  0.816  1.141
# [5,] -0.545 0.523 1.368 -0.343 -0.012

#####   ATTENTION: MAH REMOVE THIS BIT OF INFO WHEN YOU ARE CLEANING THINGS UP!!!!!!




#here is the function that will set up our plants
setup.plants <- function(repro, survive, comp.mat, name=NULL){
  if (is.null(name))
    name <- letters[seq_along(repro)]
  if (length(repro) !=length(survive))
    stop("Reproduction and survival parameters needed for all species!")
  if (length(name) != length(repro))
    stop("The number of names doesn't match reproduction and survival parameters")
  repro <- setNames(repro, name)
  survive <- setNames(survive, name)
  #set.Names is a convenience function that sets the names on an object and returns the object
  #set.Names() is the most useful at the end of a function definition where one is creating the object to be returned
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, name=name))
}

info <- setup.plants(repro, survive, setup.plants, name)
print(info)


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
plant.timestep <- function(plants, info){
  #define survivor function
  survive <- function(cell, info){
    if(is.na(cell))     #if it isnt a species I want you to return what is already in the contents of the cell
      return(NA)
    if(cell=='')
      return('')
    if(runif(1) <= info$survive[cell])   #your value is greater than or equal to your survival probability then you win yay!
      return(cell)
    if(runif(1) >= info$survive[cell])   #if the random number is greater that our survival probablity the return a blank space
      return('')    #this makes sense because if it dies it's no longer there...there is nothing in this cell
  }
  #looping through the plant matrix
  for(k in 1:(dim(plants)[3]-1)){
    for(i in 1:dim(plants)[1]){
      for(j in 1:dim(plants)[2]){
      plants[i,j,k] <- survive(plants[i,j,k], info)
      print(c(i,j,k))
      }
    }
  }
  return(plants)
}


plant.timestep(plants, info)


#timestep for plant
plant.timestep <- function(eco,info){
  for(k in 1:(dim(eco)[3]-1)){
    for(i in 1:dim(eco)[1]){
      for(j in 1:dim(eco)[2]){
        eco[i,j,k+1] <- survive(eco[i,j,k], info)
      }
    }
  }
  return(eco)
}







#6.2.3 run.plant.ecosystem---Here is where we actually make our plant array that we input into the previously generated functions
#info was generated withthe setup.plants

#what do we want here???
#'we have already written our plant.timestep function in the previous step
#'The plant.timestep function was written as if the user had already generated their terrain matrix (which is what we did in lesson one of r-world)
#'But here, we still have to seed our initial plant matrix with their starting plants
    #' We also need to get some plant parameter inforation from our user

#' information about our plants NEEDS to be stored as an array
#' An array is like a matrix (row,col) EXCEPT we want to add in an additional depth (here it is TIME)
#' This third demension will record how are plants are changing through time
      #' first load of plants plants[,,1] 
      #' second load of plants plants[,,2] 
          #' SEE HOW WE ARE SPECIFYING WHICH DEPTH! THIS IS HOW THEY ARE CHANGING OVER TIME: 1 INITIAL, 2 AFTER TIME INTERVAL???
#' Something we need to do here is randomly add the number of individuals that the user wants into the matrix at RANDOM
    #'Then go in to the matrix AFTERWARDS and make NA plants that happened to land on water
    #'To do this make sure you keep track of where the water is in your matrix
    #'This is where we want to reference our terrain to make the NA adjustments, NOT LATER! 


#making the plants array
#notice the timesteps+1
    #'Why did we do this this way?
    #'This is what should make it move through time???  plus one means it will add to the time step before
    #'


plants <- array("", dim=c(dim(Test.Terrain), 2 + 1))
for(i in seq_len(dim(plants)[3]))
  #seq_len(y) or in our case (seq_len(dim(plants)) is creating a sequence up dimensions of plants array
  plants[,,i][is.na(Test.Terrain)] <- NA



#Now we need to build this into a function
#This will be the run.plant.ecosystem

#######working up until i get to looping through the plant.timestep

run.plant.ecosystem <- function(terrain, num.timesteps, info){
  terrain <- terrain
  info <- info
  #Make the array
  plants <- array("", dim=c(dim(terrain), num.timesteps + 1))
  # for(i in 1:(.5*(nrow(terrain)*ncol(terrain))))   #######This doesn't work
  #below is how you RANDOMLY SEED YOUR PLANT MATRIX!!!!
  for(i in 1:(.5*nrow(terrain)^2)){
    plants[sample(nrow(plants),1), sample(ncol(plants),1), 1] <- sample(info$name, 1)
  }
  for(i in seq_len(dim(plants)[3])){
    #seq_len(y) or in our case (seq_len(dim(plants)) is creating a sequence up dimensions of plants array
    plants[,,i][is.na(terrain)] <- NA
  }
  plants[,,i] <- plant.timestep(plants, info)
        return(plants)
}

run.plant.ecosystem(Test.Terrain, 3, info)



#REPRODUCTION
#' our plants keep dying out....because we havent told them to reproduce yet
#' we need them to reproduce like the would in nature (we have already included their reproduction probs in the info section)
#' 
#' We need to write a function that is called "reproduce"
#' within this function we want to add a call for the plant.timestep function
#' 
#' first line should look something like this 
      #'     plant <- reproduce(row, col, plants, info)
      #' plants is key here.  this is the matrix that we generated that includes the depth of time
      #' so the ENTIRE plant matrix has to be passed
      #'
      #' Notice, we also have the info argument here
      #' "info" was generated in the setup.plants function
      #' this argument contains information such as probability of reproduction and survival, as well as the competition matrix (or probablity of success when faced with competition)
      
#'What do we want out of this function????
#'we want to call the inputs described above
#'we also want to define where they can and can't reproduce--> specifically that they can't reproduce in water
#'we also want to flter out which ones are NOT water logged and then we want to reproduce there
#'we need to CHECK that we actually have a place for them to reproduce to
#'finally we want to return plants matrix 

#reproduce function
reproduce <- function(row, col, plants.matrix, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  #filter out NOT water logged locations and then we want to reproduce here
  for(i in possible.locations){
    for(j in possible.locations){
      if(!is.na(possible.locations[i,j]))         #filtering out those that arent NA
        if(runif(1) <= info$survive[plant]) 
          plants[i,j] <- info$name[plant]   #I have no clue what to put in here?????
    }      
  }   
}

#I have no clue if it is working


















plant <- reproduce(row, col, plants, info)