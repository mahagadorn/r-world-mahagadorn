##Here is the simulation when adding an herbivore species to our plant ecosystem
##MAHagadorn
##Herbivore
##November 9, 2016


#' Simulation of a herbivore species on a matrix of a plant ecosystem
#' 
#' @param herbivore; character vector of length 1 that represents the herbivore species used in the simulation
#'     If no species is specified the default will be labeled as "herb".
#'        
#' @param repro; numeric vector of length one representing probability of reproduction for herbivore species.
#'     Reproduction probability should be between zero and one.
#'     Zero and one represent probabilites of reproduction that correspond to no chance and 100% chance of reproduction, respectively.
#'     Default value is set to .5.
#'     
#' @param kill; numeric vector of length one representing probability of the herbivore killing the plant species it is feeding upon.
#'     See variable repro for a description of probabilities ranges.
#'     Default value is set to .5.


#'In this lesson we will be simulating moving animals into our ecosystem
#'Our herbivores are going to be eating our plants created in the last session
#'the effects of our herbivores on our plant populations will be quite dramatic
#'
#'THREE LIFE HISTORY PARAMETERS FOR OUR HERBIVORES
    #' Probabilites for:
        #' eating a plant 
        #' killing a plant when they eat
        #' reproducing once they have eaten
    #'Herbivores wont feed every time-step
    #'When they don't feed they're going to move
    #'Whether an herbivore feeds or not will be related to how long since they have last fed
    #'IF THEY GO TOO LONG WITHOUT FEEDING THEY WILL DIE
#' we need to make sure that we give some DEEP though to what DATA STRUCTURE we use to store our herbivores



#Herbivore setup.herbivore!!!!!

setup.herbivores <- function(eat=eat, kill=.5, repro=.5, herbivore=c("herb")){
  if(!is.numeric(eat) | !is.numeric(kill) | !is.numeric(repro))
    stop("Herbivores need numeric data")
  if(length(kill) != 1)
    stop("Herbivores need one kill probability")
  if(length(repro) != 1)
    stop("Herbivores need one reproduction probability")
  sated <- length(eat)
  eat <- setNames(eat, herbivore)
  kill <- setNames(kill, herbivore)
  repro <- setNames(repro, herbivore)
  return(list(eat=eat, kill=kill, repro=repro, sated=sated))
}


info <- setup.herbivores(eat, kill, repro, herbivore)



#HERBIVORE STORAGE=MATRIX FORMAT

#probability that our herbivore will reproduce
repro <- .50

#survival vector containing probability of survival for 3 different species
kill <- .25

herbivore <- c("Bos taurus")



#let's set up the movement aspect to our herbivore
###### THIS IS GOING TO BE SIMILAR TO THE DISPERSAL (REPRO) FUNCTION IN OUR PLANTS. ONCES YOU GET THAT WORKING IMPLEMENT HERE

new.loc <- function(row, col, herbivore){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  #filter out NOT water logged locations and then we want to reproduce here
  for(i in possible.locations){
    for(j in possible.locations){
      if(!is.na(possible.locations[i,j]))         #filtering out those that arent NA
        if(runif(1) <= ???[herbivore]) 
          herbivore[i,j] <- ????[herbivore]   #########THIS IS NOT RIGHT!!!!!!!!
    }      
  }   
}

#Eating section
#' Eating is based on the time to death
#' DEATH IS ZERO

#Sated = 5 @ five they are good to go....they have fie more time steps until they die
eat <- c(.1, .3, .5, .7, .9)   #probability of eating

eat.fun <- function(herbivore, sated){
  eat <- c(.1, .3, .5, .7, .9)
  sated <- sated
    prob.ind <- eat[herbivore[row,col]]
}
  eat <- c(.1, .3, .5, .7, .9)



