
# 1 Modified the the R bootstrapping code to be more efficient. 
#   It is also be parallelised at some level. 
#   Profiled both the original and final versions as well as determining the overall speed increase. 
#   Included the profile file in my repository.
# 2 The new bootstrap function in R is altered to accept an arbitrary number of covariates.
# 3 Modifications to the bootstrap function is version controlled via your project GitHub repository.
# 4 Micro-benchmark (package microbenchmark) the R bootstrap against bootstraps via the package boot.


# Import the data
fitness <- read.csv("~/Desktop/5763- A2/Software-Proj-2/data/fitness.csv")

# Load the package
library(boot)

# Create data and set seed to ensure results are reproducible
x <- fitness$Age
y <- fitness$Oxygen
regData <- data.frame(x, y)

################

# The function to pass boot
lmBoot <- function(inputData, nBoot) {
  
  for(i in 1:nBoot){
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    # fit the model under this alternative reality
    bootLM <- lm(y ~ x, data = bootData)
    
    # store the coefs
    if(i == 1){
      bootResults <- matrix(coef(bootLM), ncol = 2)
    } else {
      bootResults<- rbind(bootResults, matrix(coef(bootLM), ncol = 2))
    }
    
  } 
  
  # end of i loop
  
  bootResults
}

system.time(test <- lmBoot(regData, nBoot = 100000))

##############

# Parallelising

library(parallel)

detectCores()

nCores <- detectCores()
nCores

myClust <- makeCluster(nCores-1, type = "FORK")

clusterEvalQ(myClust, library(boot))

sampleData <- fitness

clusterExport(myClust, "fitness")
clusterEvalQ(myClust, dataSum <- sum(fitness))

dataSum <- clusterEvalQ(myClust, sum(fitness))
dataSum

################ improved code

lmBoot1 <- function(inputData, nBoot) {
  
  x <- cbind(1, scale(inputData$x, scale = F))
  y <- scale(inputData$y, scale = F)
  scaleData <- as.matrix(cbind(x, y))
  
  bootResults <- array(dim=c(nBoot, 2))
  
  for(i in 1:nBoot){
    
    # resample our data with replacement
    bootData <- scaleData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    xmat <- bootData[,1:2]
    ymat <- bootData[,3]
    
    # fit the model under this alternative reality
    # Changed the lm part to matrix form
    
    beta <- solve(t(xmat) %*% xmat) %*% t(xmat) %*% ymat
    
    bootResults[i,] <- beta
    
  } # end of i loop
  
  bootResults
  
}

system.time(lmBoot1(regData, nBoot = 100000))


################## Using foreach

library('foreach')

lmBoot2 <- function(inputData, nBoot) {
  
  x <- cbind(1, scale(inputData$x, scale = F))
  y <- scale(inputData$y, scale = F)
  scaleData <- as.matrix(cbind(x, y))
  
  bootResults <- array(dim=c(nBoot, 2))
  
  foreach(i = 1:nBoot)  %dopar% {
    
    # resample the data with replacement
    bootData <- scaleData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    xmat <- bootData[,1:2]
    ymat <- bootData[,3]
    
    # fit the model under this alternative reality
    # Changed the lm part to matrix form
    
    beta <- solve(t(xmat) %*% xmat) %*% t(xmat) %*% ymat
    
    bootResults[i,] <- beta
    
  } # end of i loop
  
  bootResults
  
}

system.time(lmBoot2(regData, nBoot = 100000))

################# Efficient

parBadBoot <- function( index, scaleData, N )
{
  bootData <- scaleData[sample( 1:N, N, replace = T ),]
  Xmat <- bootData[,1:2]
  Ymat <- bootData[,3]
  
  # fit the model under this alternative reality
  # Changed the lm part to matrix form
  beta <- solve( t( Xmat ) %*% Xmat ) %*% t( Xmat ) %*% Ymat
  bootResults <- beta
}

# final bootstrap function that utilises parLapply function to speed
# up process
bestBadBootBrother <- function( inputData, nBoot )
{
  x <- cbind( 1, scale( inputData$x, scale = F ) )
  y <- scale( inputData$y, scale = F )
  scaleData <- as.matrix( cbind( x, y ) )
  
  bootResults <- array( dim = c( nBoot, 2 ) )
  
  N <- nrow( inputData )
  
  tempbootResults <- parLapply( myClust, 1:nBoot, parBadBoot, 
                                scaleData = scaleData, N = N ) 
  
  bootResults <- tempbootResults 
  
  r <- bootResults[[1]]
  c <- bootResults[[2]]
  m <- matrix( c( r, c ), ncol = 2, nrow = N )
  
  return( m ) 
}

system.time( test2 <- bestBadBootBrother( inputData = regData, 100000 ) )


# Plotting of time --------------------------------------------------------
set.seed(1435)
plotting <- data.frame(matrix(0, nrow = 7, ncol = 2))

plotting[1,1] <- "lmBoot"
i.1 <- system.time(lmBoot(regData, nBoot = 10000))
plotting[1,2] <- i.1[["elapsed"]]
  
plotting[2,1] <- "lmBoot1"
i.2 <- system.time(lmBoot1(regData, nBoot = 10000))
plotting[2,2] <- i.2[["elapsed"]]

plotting[3,1] <- "lmBoot2"
i.3 <- system.time(lmBoot2(regData, nBoot = 10000))
plotting[3,2] <-  i.3[["elapsed"]]

plotting[4,1] <- "bestBadBootBrother"
i.4 <- system.time( test2 <- bestBadBootBrother( inputData = regData, 10000 ) )
plotting[4,2] <-  i.4[["elapsed"]]

plotting[5,1] <- "bestBadBootBrotherAnyCovars1"
i.5 <- system.time( test3 <- bestBadBootBrotherAnyCovars1( 10000, regData$y, regData$x) )
plotting[5,2] <- i.5[["elapsed"]]

plotting[6,1] <- "bestBadBootBrotherAnyCovars2"
i.6 <- system.time( test3 <- bestBadBootBrotherAnyCovars2( 10000, regData$y, regData$x) )
plotting[6,2] <- i.6[["elapsed"]]

plotting[7,1] <- "bestBadBootBrotherAnyCovars3"
i.7 <- system.time( test3 <- bestBadBootBrotherAnyCovars3( 10000, regData$y, regData$x) )
plotting[7,2] <- i.7[["elapsed"]]


ggplot( data = plotting, aes( x = X1, y = X2,fill = factor( X1 ))) +
  geom_bar( position = "dodge", stat = "identity" ) + 
  coord_flip() +
  scale_x_discrete(
    limits=c( "bestBadBootBrotherAnyCovars3", "bestBadBootBrotherAnyCovars2", 
              "bestBadBootBrotherAnyCovars1","bestBadBootBrother", "lmBoot2", 
              "lmBoot1", "lmBoot") ) +
  ggtitle("Time (in seconds) for the versions of the function to run 10 000 iterations") +
  ylab("Time") + xlab("Funnction version") + theme(legend.position="none")



##################















