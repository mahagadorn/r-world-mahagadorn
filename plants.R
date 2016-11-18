##Simulates a plant ecosystem that incorperates species, reproduction probabilities, survival probabilites, and competition probabilities.
##MAHagadorn
##Plants
##November 7, 2016


#The first thing that we need to do is write a defensive function
#this function will check a variety of things to ensure that all the input information is right
    ##basically we are checking to make sure the user isn't putting in any information that is wrong


#' Step up a list of plant information
#'
#' This function generates a list of properties associated with each plant species used in the simulation.
#' @param repro Numeric vector of length two representing probabilities of reproduction for the corresponding species (Default: 0.5).
#'    Reproduction probabilities should be between zero and one.
#'    Zero represents a probability of reproduction equal to zero, or no chance of reproducing.
#'    One represents a probability of reproduction equal to one, or 100% chance of reproduction.
#'    In relation to \code{name}, repro vector positions 1 and 2 correspond to species "a" and "b", respectively.
#' @param survive Numeric vector of length two representing probabilites of survival for the corresponding species (Default: 0.5).
#'    See variable repro for a description of probability ranges.
#'    In relation to \code{name}, survive vector positions 1 and 2 correspond to species "a" and "b", respectively.
#' @param comp.mat 2 by 2 matrix that corresponds to individual probabilites of a species winning the competition between another species
#'    There is no default for this input; therefore, the user must define this variable.
#' @param name Character vector of length two that incorperates the two plant species names to be used in the simulation (Default: "a", "b")
#' @examples
#'    setup.plants <- function(repro=c(.5,.5), survive=c(.5,.5), comp.mat, name=c("a", "b"))
#' @author Mallory Hagadorn
#' @return list of elements corresponding to argument inputs

#here is the function that will set up our plants
setup.plants <- function(repro=c(.5,.5), survive=c(.5,.5), comp.mat, name=c("a", "b")){
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
  return(info<-list(repro=repro, survive=survive, comp.mat=comp.mat, name=name))
}







#' Make empty array
#'
#' Pre-allocate an array which corresponds to the dimensions of terrain and the number of times steps to iterate through.
#' @param terrain A matrix containing numeric elements that are indicates as heights and if lake.na=TRUE \code{NA}s indicated cells that are waterlogged. Terrain matrix is visualized using \code{image}.
#' @param num.timesteps Numeric value indicating the number of times steps that should be iterated.
#' @author Mallory Hagadorn
#' @return an array called "plants"

mk.plant.array <- function(terrain, num.timesteps=5){
plants <- array("", dim=c(dim(terrain), num.timesteps + 1))  #Generating the plant array with the number of time steps + 1.  The plus one is how we move to next step.
return(plants)
}







#' Determines if the input (where appropriate) will survive
#'
#' This function takes the specific cell of an input matrix (can be NA, '', or name) and determines if that cell will "survive" to the next time step.
#' Inputs of NA represent a waterlogged cell, whereas those indicated with '', represent cells that are unoccupied.
#' When a cell is filled with a specific name, that name is referenced back to the list created in \code{setup.plants}. This list contains survival probabilites, which are compared to a random number generated from a uniform distribution.
#' If what is in the cell survives, it will be passed to subsequent time series.
#' @param cell A specific cell [row, column] in a matrix.
#'    The survive function will be included in a \code{plant.timestep} function, which is applied to an array.
#' @param info A list including reproduction, survival, and competition probabilities, as well as, species names.
#'    This list is generated using the \code{setup.plants} function.
#' @author Mallory Hagadorn
#' @return single element of a matrix

survive.fun <- function(cell, info){
  random <- runif(1)  #Generate one random number from a uniform distribution (this will plug into our if statements when plants are present)
  if(is.na(cell))     #If cell is water (NA), return NA;thus, keeping it water
    return(NA)
  if(cell=='')        #If the cell is blank, return a blank cell
    return('')
  if(random <= info$survive[cell])   #your value is greater than or equal to your survival probability then you win yay!
    return(cell)
  if(random >= info$survive[cell])
    return('')   #this makes sense because if it dies it's no longer there...there is nothing in this cell
}








#REPRODUCTION
# our plants keep dying out....because we havent told them to reproduce yet
# we need them to reproduce like the would in nature (we have already included their reproduction probs in the info section)

# We need to write a function that is called "reproduce"
# within this function we want to add a call for the plant.timestep function

# first line should look something like this
# plant <- reproduce(row, col, plants, info)
# plants is key here.  this is the matrix that we generated that includes the depth of time
# so the ENTIRE plant matrix has to be passed

# Notice, we also have the info argument here
# "info" was generated in the setup.plants function
# this argument contains information such as probability of reproduction and survival, as well as the competition matrix (or probablity of success when faced with competition)

# What do we want out of this function????
# we want to call the inputs described above
# we also want to define where they can and can't reproduce--> specifically that they can't reproduce in water
# we also want to flter out which ones are NOT water logged and then we want to reproduce there
# we need to CHECK that we actually have a place for them to reproduce to
# finally we want to return plants matrix


#' Simulation of reproduction
#'
#' This function take the probabilities included in our \code{info} list and applies them to the elements in the array to determine if that element will successfully reproduce or not.
#' @param row Row location
#' @param col Column location
#' @param plants An array of plant matrice with time as the depth dimension.
#'     Array generated using the \code{run.plant.ecosystem}.
#' @param time.step Numeric value indicating the number of times steps that should be looped over (Default: 5).
#'    Maximum number of iterations is 1000.
#' @param info A list including reproduction, survival, and competition probabilities, as well as, species names.
#'    This list is generated using the \code{setup.plants} function.
#' @author Mallory Hagadorn
#' @return an array called "plants"

reproduce.fun <- function(row, col, time.step, plants, info){
  #creating possible locations based on the specific row and column location
  poss.locs <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  #filter out NOT water logged locations and then we want to reproduce here
  #row and column need to be our specific positions
  #indexes already used = i, j ,k
  random <- runif(1)    #Learned this trick from Will and my Survive function! We have to specify JUST ONE random number to use.
  for(m in 1:nrow(poss.locs)){       #looping through the index ranging from 1 through the number of rows in the possible locations matrix made above
    col.1 <- poss.locs[m,1] #Recieved help from Maggi K. (@MaggiK) on november 17th
    #What's happening here: we are saving the values for the i-th row and the first column of the matrix containing all possible locations
    col.2 <- poss.locs[m,2] #Recieved help from Maggi K. (@MaggiK) on november 17th
    #What's happening here: we are saving the values for the i-th row and the second column of the matrix containing all possible locations
    #col.1 and col.2 values will be fed back into an if statement that is WITHIN the for loop!
    if(col.1 <= ncol(plants) & col.2 <= ncol(plants) & plants[row,col, time.step] != '' & !is.na(plants[row,col, time.step]) & col.1 > 0 & col.2 > 0){
      #THIS IS WHERE I GOT QUITE A BIT OF HELP FROM MAGGI K
      #What this is saying: The above statement is broken up into multiple "#" commented statements
      #IF both the values of col.1 (see description above) AND col.2 are less than the number of columns in our plant array
      #AND if the subsetted cell of our plant array (plants[row,col,k]) is not a blank space ('')...A blank space can't reproduce
      #AND if the subsetted cell isn't waterlogged (NA)...Water can't reproduce
      #AND if our col.1 and col.2 values are greater that ZERO....
      if(!is.na(plants[col.1, col.2, time.step]) & plants[col.1, col.2, time.step] == ''){
        #This is specific to the particular location
        #I had something similar to the first part (was calling i and j which wasnt working out), but I was missing the second part.
        #The second part I talked through with Maggi K
        #What this if statement is saying: if the cell doesn't equal waterlogged (can't reproduce here) AND it IS BLANK! (aka places we can actually reproduce)
        if(random <= info$repro[plants[row, col, time.step]]){    #similar to survive function: if our random value is less than the reproduction probability at the specified location in the ARRAY
          #Had this right I just needed to specify a SINGLE Random value
          #MAH Note: It is important to keep nesting these loops
          #that means we are still looping through all the above loops!
          plants[col.1, col.2, time.step+1] <- plants[row, col, time.step]  #So if our random value is less than or equal to repro prob ( = reproduction) then take that plant name and put it into the k time step!
          #I didn't specify time.step+1 because I do that in my plant.timestep function
        }
      }   #Closing the second if statement
    }     #Closing the first if statement
  }
  return(plants)
}









#now we need a function that makes time "tick" by for an entire matrix of plants
#this function should have an input that is our plant matrix
#we want to loop over the plant matrix
#then we want to apply the survivor function to all of the contents
#what we want is a loop for the plant matrix with the survive function applied to it.


#' Simulate plant ecosystem for distinct points in time
#'
#' Function that incorperates intervals of time and for our entire plant matrix
#' @param plants An array of plant matrice with time as the depth dimension.
#'     Array generated using the \code{run.plant.ecosystem}.
#' @param info A list including reproduction, survival, and competition probabilities, as well as, species names.
#'    This list is generated using the \code{setup.plants} function.
#' @author Mallory Hagadorn
#' @return an array called "plants"

#plant.timestep function
plant.timestep <- function(plants=plants, info=info){
  #looping through the plant matrix
  for(k in 1:(dim(plants)[3]-1)){       # for index in the array depth
    for(i in 1:(dim(plants)[1])){       # for index in the matrices rows that make up the ovarall array
      for(j in 1:(dim(plants)[2])){     # for index in the matrices columns that make up the ovarall array
        plants[i,j,(k+1)] <- survive.fun(plants[i,j,k], info)   #calls the survive function and saves the results into the plant array at timestep K+1 (meaning the next time step)
        plants <- reproduce.fun(i, j, k, plants, info)
      }
    }
  }
  return(plants)
}


###ERROR MESSAGE: Troubleshooting for future use.
# Error in plants[i, j, k] <- survive(plants[i, j, k], info) :
# number of items to replace is not a multiple of replacement length
# Was receiving this error because I made a simple mistake.  I fed two numbers generated from a random uniform dist (runif) in my survive function
# This mean was trying to feed in more numbers then it was supposed to...hense the replace is not a multiple of replacement length
# This was fixed by saving a single number generated with runif() [called random in survive function] and feeding that into the steps of the function.
# Got assistance on this from W. Pearse.













# run.plant.ecosystem---Here is where we actually make our plant array that we input into the previously generated functions
# Info was generated with the setup.plants

# What do we want here???
# We have already written our plant.timestep function in the previous step
# The plant.timestep function was written as if the user had already generated their terrain matrix (which is what we did in lesson one of r-world)
# But here, we still have to seed our initial plant matrix with their starting plants  (can be done using sample())
# We also need to get some plant parameter information from our user

# Information about our plants NEEDS to be stored as an array (THIS IS A MUST!!)
# An array is like a matrix (row,col) EXCEPT we want to add in an additional depth (here it is TIME)
# This third demension will record how are plants are changing through time
# first load of plants plants[,,1]
# second load of plants plants[,,2]
# SEE HOW WE ARE SPECIFYING WHICH DEPTH! THIS IS HOW THEY ARE CHANGING OVER TIME: 1 INITIAL, 2 AFTER TIME INTERVAL.
# Something we need to do here is randomly add the number of individuals that the user wants into the matrix at RANDOM
# Then go in to the matrix AFTERWARDS and make NA plants that happened to land on water
# To do this make sure you keep track of where the water is in your matrix
# This is where we want to reference our terrain to make the NA adjustments, NOT LATER!


#making the plants array
#notice the timesteps+1
# Why did we do this this way?
# This is what should make it move through time.

#Now we need to build this into a function
#This will be the run.plant.ecosystem


#' Simulation of plant ecosystem through time
#'
#' A function that generates, seeds, and places NA's in the array where they correspond in the terrain matrix.  Applies survice, reproduction, and competition functions.
#' @param terrain Numeric matrix generated using \code{make.terrain}
#' @param num.timesteps Numeric value indicating the number of times steps that should be looped over (Default: 5).
#'    Maximum number of iterations is 1000.
#' @param info A list including reproduction, survival, and competition probabilities, as well as, species names.
#'    This list is generated using the \code{setup.plants} function.
#' @return a plant array
#' @author Mallory Hagadorn

run.plant.ecosystem <- function(terrain=terrain, num.timesteps=5, info=info){
  if(num.timesteps > 1000){       #This will keep us from going into an infinite loop! So if it's greater than 1000 timesteps I want you to "stop" and return a warning.
    return("To many time steps")
  } else{
    #Make the array
    plants <- array("", dim=c(dim(terrain), num.timesteps + 1))   #Generating the plant array with the number of time steps + 1.  The plus one is how we move to next step.
    #below is how you RANDOMLY SEED YOUR PLANT MATRIX!!!!
    for(k in 1:(.5*nrow(terrain)^2)){     #This is saying for some index for a range between 1 and half the number of rows in our terrain matrix^2.  The Square aspect gets us a "pseduo" length for both rows and columns
      plants[sample(nrow(plants),1), sample(ncol(plants),1), 1] <- sample(info$name, 1)   #Alex Rego and I worked on this to randomly "sample" or place our starting plants in out plant [,,1]
    }
    for(k in seq_len(dim(plants)[3])){
      #seq_len(y) or in our case (seq_len(dim(plants)) is creating a sequence up dimensions of plants array
      plants[,,k][is.na(terrain)] <- NA      #This is saying to put NA's into our array whereever they existed in our terrain matrix.  This is important because NA represents water-logged (so plants can't there)
      #This will over write any plants that were randomly seeded in these spots when using plants[sample(nrow(plants),1), sample(ncol(plants),1), 1] <- sample(info$name, 1)
    }
    plants <- plant.timestep(plants, info)     #apply the plant.timestep to the array we just generated and save it as our "new" plants array.
    return(plants)
  }
}








#Here I have made a wrapper function that makes my terrain and simulates the entire plant ecosystem
#I have done this because it is the only way I could figure out how to generate and feed terrain into all my plant steps
#When I didn't do this I always had to save a matrix called terrain
#Will, you said that I couldn't do this because packages should ONLY every contain functions and not vectors

#' Generate a terrain matrix and simulate a plant ecosystem on that terrain.
#'
#' This is a wrapper function that will make both a terrain matrix and simulate a plant ecosystem through time on that terrain.
#'     This function should be used if you want to generate all at once.
#'     If the user simply wants to generate just a terrain (an not simulate a plant ecosystem) the user should use the function \code{make.terrain}.
#'     If the user wants to generate a terrain and simulate a plant ecosystem on that terrain they should use the function \code{terrain.plantecosystem.wrapper}
#'     The wrapper function calls \code{run.plant.ecosystem} and will simulate a plant ecosystem based on the arguments \code{repro}, \code{survive}, \code{comp.mat}, and \code{name}.
#'
#' @param n Size of the grid will be a (2^n)+1 (by default: n=5; a 33 by 33 grid)
#' @param x.bar Numeric vector (Default: 500) that will be input as the mean into the rnorm() function.
#' @param print.terrain If True the generated terrain matrix will be printed.
#' @param repro Numeric vector of length two representing probabilities of reproduction for the corresponding species (Default: 0.5).
#'    Reproduction probabilities should be between zero and one.
#'    Zero represents a probability of reproduction equal to zero, or no chance of reproducing.
#'    One represents a probability of reproduction equal to one, or 100% chance of reproduction.
#'    In relation to \code{name}, repro vector positions 1 and 2 correspond to species "a" and "b", respectively.
#' @param survive Numeric vector of length two representing probabilites of survival for the corresponding species (Default: 0.5).
#'    See variable repro for a description of probability ranges.
#'    In relation to \code{name}, survive vector positions 1 and 2 correspond to species "a" and "b", respectively.
#' @param comp.mat 2 by 2 matrix that corresponds to individual probabilites of a species winning the competition between another species
#'    There is no default for this input; therefore, the user must define this variable.
#' @param name Character vector of length two that incorperates the two plant species names to be used in the simulation (Default: "a", "b")
#' @param num.timesteps Numeric value indicating the number of times steps that should be looped over (Default: 5).
#'    Maximum number of iterations is 1000.
#' @param water Logical (Default: TRUE) that specifies whether to make all terrain values lower than zero height underwater.
#' @author Mallory Hagadorn
#' @examples
#' plant.matrix <- terrain.plantecosystem.wrapper(n=3,x.bar=100, print.terrain=TRUE, repro=c(.5,.5), survive=c(1,.5), comp.mat= as.matrix(c(.25,.5,.75,1), ncol=length(repro)), name = c("M. sativa", "L. perenne"), 3, water=TRUE)
#' @export

terrain.plantecosystem.wrapper <- function(n, x.bar, print.terrain=TRUE, repro, survive, comp.mat, name, num.timesteps, water=TRUE){
  terrain <- make.terrain(n, x.bar, water)   #You can't return more than one value from a function, so simply printing (if they want it printed).
  if(print.terrain==TRUE){
    print(terrain)
  }
  info <- setup.plants(repro, survive, comp.mat, name)
  plants <- run.plant.ecosystem(terrain, num.timesteps, info)
  return(plants)    ###This is the more important thing to be returned, so it is being returned over the terrain matrix.
}









#######ALL THE ABOVE WORK GREAT, COMPETITION WAS THE ONLY THING I COULDN'T FIGURE OUT
#I have included that code below in hopes of some partial credit!! :)     We can all hope every now and again.

#Competition function
#What we need to keep in mind here is that we are going to want our specific plants to fight
#that means we will need to include a list of our species and their probability of winning a fight compared to another species

#' Simulation of competition between two plant species
#'
#' This function take the probabilities included in our \code{info} list and applies them to the elements in the array to determine which species would win a fight between two possible species.
#' @param row Row location
#' @param col Column location
#' @param plants An array of plant matrice with time as the depth dimension.
#'     Array generated using the \code{run.plant.ecosystem}.
#' @param time.step Numeric value indicating the number of times steps that should be looped over (Default: 5).
#'    Maximum number of iterations is 1000.
#' @param info A list including reproduction, survival, and competition probabilities, as well as, species names.
#'    This list is generated using the \code{setup.plants} function.
#' @author Mallory Hagadorn
#' @return an array called "plants"

competition <- function(row, col, time.step, info, plants){ #need to tether comp.mat in plants
  comp.mat <- info$comp.mat
  name <- info$name
  plants <- plants
  random <- runif(1)
  for(k in plants){
    for(i in plants){
     for(j in plants){
       plants[i,j,(k+1)] <- sample(info$name, size=1, prob=info$comp.mat[plants[i, j, k]])   # if(random <= info$repro[plants[row, col, time.step]])
      }
    }
  }
  return(plants)
}







