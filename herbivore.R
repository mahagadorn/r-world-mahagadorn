##MAHagadorn
##R-World
##Herbivore
##November 9, 2016

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



#HERBIVORE STORAGE=MATRIX FORMAT

#probability that our herbivore will reproduce
repro <- .50

#survival vector containing probability of survival for 3 different species
kill <- .25



#let's set up the movement aspect to our herbivore
###### THIS IS GOING TO BE SIMILAR TO THE DISPERSAL (REPRO) FUNCTION IN OUR PLANTS. ONCES YOU GET THAT WORKING IMPLEMENT HERE.



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









#Herbivore setup.herbivore!!!!!

setup.herbivores <- function(eat, kill, repro){
  if(!is.numeric(eat) | !is.numeric(kill) | !is.numeric(repro))
      stop("Herbivores need numeric data")
  if(length(kill) != 1)
      stop("Herbivores need one kill probability")
  if(length(repro) != 1)
      stop("Herbivores need one reproduction probability")
  sated <- length(eat)
  return(list(eat=eat, kill=kill, repro=repro, sated=sated))
}







