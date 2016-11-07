##MAHagadorn
##R-World
##Plants
##November 7, 2016

#The first thing that we need to do is write a defensive function
#this function will check a variety of things to ensure that all the input information is right
    ##basically we are checking to make sure the user isn't putting in any information that is wrong


#need to create our input vectors
#reproduction probability vector
SpArepro <- .50
SpBrepro <- 1
SpCrepro <- .75

repro <- c(SpArepro, SpBrepro, SpCrepro)

#survival vectors
SpAsur <- .75
SpBsur <- .50
SpCsur <- .50   

survive <- c(SpAsur, SpBsur, SpCsur)

#here is the function that will set up our plants

setup.plants <- function(repro, survive, comp.mat, names=NULL){
  if (is.null(names))
    names <- letters[seq_along(repro)]
  if (length(repro) !=length(survive))
    stop("Reproduction and survival parameters needed for all species!")
  #some more tests....
  repro <- setNames(repro, names)
  #set.Names is a convenience function that sets the names on an object and returns the object
  #set.Names() is the most useful at the end of a function definition where one is creating the object to be returned
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}


#First Steps: storing plants and keeping them alive!!

#Survival function

survive <- function(cell, info){
  
}