Test.Terrain <- matrix(NA, nrow=5, ncol=5)


Test.Terrain[1,] <- round(rnorm(5), digits = 3)
Test.Terrain[2,] <- round(rnorm(5, 3, 1), digits = 3)
Test.Terrain[3,] <- round(rnorm(5, 2, 1), digits = 3)
Test.Terrain[4,] <- round(rnorm(5, 1, .25), digits = 3)
Test.Terrain[5,] <- round(rnorm(5, 1, 1), digits = 3)

#Lets add in som NA's so that we can test the NA aspect of our functions

Test.Terrain[4,3] <- NA
Test.Terrain[3,3] <- NA
