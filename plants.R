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
  #set.Names is a convenience function that sets the names on an object and returns the object
  #set.Names() is the most useful at the end of a function definition where one is creating the object to be returned
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}

setup.plants(repro, survive, setup.plants, names)

#just testing to make sure that the added names section works out.
# test.name<-c("A", "B")
# setup.plants(repro, survive, setup.plants, test.name)

#First Steps: storing plants and keeping them alive!!

#Survival function
#this determines whether a particular species will survive
survive <- function(cell, info){
  for
}











