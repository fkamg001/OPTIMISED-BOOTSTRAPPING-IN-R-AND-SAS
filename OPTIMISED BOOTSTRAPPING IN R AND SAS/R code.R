
# 1 Modified the the R bootstrapping code to be more efficient. 
#   It is also parallelised at some level. 
#   Profiled both the original and final versions as well as determining the overall speed increase. 
#   Included the profile file in the repository.
# 2 The new bootstrap function in R is altered to accept an arbitrary number of covariates.
# 3 Modifications to the bootstrap function is version controlled via my project GitHub repository.
# 4 Micro-benchmark (package microbenchmark) the R bootstrap against bootstraps via the package boot.


# Import the data
fitness <- read.csv("~/Desktop/5763- A2/Software-Proj-2/data/fitness.csv")
set.seed(1435)

# Load the package
install.packages("boot")
library("boot")

# Create data and set seed to ensure results are reproducible
x <- fitness$Age
y <- fitness$Oxygen
regData <- data.frame(x, y)

# function to pass boot

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

# Parallelising

library("parallel")

detectCores()

nCores <- detectCores()
nCores

myClust <- makeCluster(nCores-1, type = "PSOCK")

clusterEvalQ(myClust, library(boot))

sampleData <- fitness

clusterExport(myClust, "fitness")
clusterEvalQ(myClust, dataSum <- sum(fitness))

dataSum <- clusterEvalQ(myClust, sum(fitness))
dataSum

# Improved codes

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


# Using foreach
install.packages("foreach")
library("foreach")

lmBoot2 <- function(inputData, nBoot) {
  
  x <- cbind(1, scale(inputData$x, scale = F))
  y <- scale(inputData$y, scale = F)
  scaleData <- as.matrix(cbind(x, y))
  
  bootResults <- array(dim=c(nBoot, 2))
  
  foreach(i = 1:nBoot)  %dopar% {
    
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

system.time(lmBoot2(regData, nBoot = 100000))

# Make the function more efficient by using a helper function
parBadBoot <- function( index, scaleData, N )
{
  # sample data with replacement
  bootData <- scaleData[sample( 1:N, N, replace = T ),]
  Xmat <- bootData[,1:2]
  Ymat <- bootData[,3]
  
  # fit the model under this alternative reality
  # Changed the lm part to matrix form
  beta <- solve( t( Xmat ) %*% Xmat ) %*% t( Xmat ) %*% Ymat
  bootResults <- beta
}

# Bootstrap function that utilises parLapply function to speed up the process
bestBadBootBrother <- function( inputData, nBoot )
{
  # parallelize
  library(parallel)
  nCores <- detectCores()
  myClust <- makeCluster(nCores-1, type = "PSOCK")
  # scale inputData and pass to helper function in form of a matrix
  x <- cbind( 1, scale( inputData$x, scale = F ) )
  y <- scale( inputData$y, scale = F )
  scaleData <- as.matrix( cbind( x, y ) )
  
  bootResults <- array( dim = c( nBoot, 2 ) )
  
  N <- nrow( inputData )
  
  tempbootResults <- parLapply( myClust, 1:nBoot, parBadBoot, 
                                scaleData = scaleData, N = N ) 
  
  bootResults <- tempbootResults 
  # save all bootstap data into matrix and output
  r <- bootResults[[1]]
  c <- bootResults[[2]]
  m <- matrix( c( r, c ), ncol = 2, nrow = N )
  
  return( m ) 
}

system.time( test2 <- bestBadBootBrother( inputData = regData, 100000 ) )

# Extending the above helper function to accept an arbitraty number of covariates
parBootAnyCovars <- function( index, scaleData, N )
{
  # sample data with replacement
  bootData <- scaleData[sample( 1:N, N, replace = T ),]
  Xmat <- bootData[,1:(ncol(bootData)-1)]
  Ymat <- bootData[,ncol(bootData)]
  # fit the model under this alternative reality
  # Changed the lm part to matrix form
  beta <- solve( t( Xmat ) %*% Xmat ) %*% t( Xmat ) %*% Ymat
  return(beta)
}

# Include covariates by looping through
bestBootCovars1 <- function( nBoot, yDat, ... ) 
{
  # parallelize
  library(parallel)
  nCores <- detectCores()
  myClust <- makeCluster(nCores-1, type = "PSOCK")
  # save x covariates into list and loop through them to constuct covariate matrix
  xDat <- list(...)
  x <- cbind(1, xDat[[1]])
  if(length(xDat) > 1) {
    for(i in 2:length(xDat)) {
       x <- cbind(x,xDat[[i]])
    }
  }
  
  #y <- scale( yDat, scale = F )
  
  # unite x covariates and y into matrix
  scaleData <- cbind( x, yDat )
  
  N <- length( yDat ) 
  
  # pass scaled data to helper function to bootstrap
  tempbootResults <- parLapply( myClust, 1:nBoot, parBootAnyCovars, 
                                scaleData = scaleData, N = N ) 
  # save all bootstap data into matrix and output
  m <- matrix( unlist(tempbootResults), ncol = length(xDat)+1, byrow=T)  
  return( m ) 
}

# Changing the for loop on xDat into something more dynamic
bestBootCovars2 <- function( nBoot, yDat, ... ) 
{
  # parallelize
  library(parallel)
  nCores <- detectCores()
  myClust <- makeCluster(nCores-1, type = "PSOCK")
  # save x covariates into list
  xDat <- list(...)
  # create matrix with x covariates and y and scale it
  mdata <- unlist(xDat)
  mcol <- length(xDat)
  mrow <- length( mdata ) / mcol
   
  baseone <- rep( 1 , mrow)
   
  mnew <- matrix( data = c(mdata, yDat), nrow = mrow, ncol = mcol+1)
  #mscale <- scale( mnew, scale = F )
  scaleData <- matrix( data = c( baseone, mnew ), nrow = mrow, ncol = mcol + 2 )
  
  N <- length( yDat ) 
  # pass scaled data to helper function to bootstrap
  tempbootResults <- parLapply( myClust, 1:nBoot, parBootAnyCovars, 
                                scaleData = scaleData, N = N ) 
  # save all bootstap data into matrix and output 
  m <- matrix( unlist(tempbootResults), ncol = length(xDat)+1, byrow=T)
  
  return( m ) 
}

# Include covariates via matrix
bestBootCovars3 <- function( nBoot, yDat, ... ) 
{
  # parallelize
  library(parallel)
  nCores <- detectCores()
  myClust <- makeCluster(nCores-1, type = "PSOCK")
  
  # save x covariates into list, and turn them into a scaled matrix in one step
  xDat <- list(...)
  #x <- cbind(1, scale(do.call(cbind, xDat), scale =F ))
  x <- cbind(1, do.call(cbind, xDat))
  #y <- scale( yDat, scale = F )

  scaleData <- cbind(x, y)
  
  N <- length( yDat ) 
  
  # pass scaled data to helper function to bootstrap
  tempbootResults <- parLapply( myClust, 1:nBoot, parBootAnyCovars, 
                                scaleData = scaleData, N = N ) 
  # save all bootstap data into matrix and output
  m <- matrix( unlist(tempbootResults), ncol = length(xDat)+1, byrow=T)
  
  return( m ) 
}

# Fastest version by scaling data in one step
oneBootToRuleThemAll <- function( nBoot, yDat, ... ) 
{
  # parallelize
  library(parallel)
  nCores <- detectCores()
  myClust <- makeCluster(nCores-1, type = "PSOCK")
  
  # save x covariates into list
  xDat <- list(..., yDat)
  # create data matrix in one step
  scaleData <- cbind(1, do.call(cbind, xDat))
  N <- length( yDat ) 
  
  # pass scaled data to helper function to bootstrap
  tempbootResults <- parLapply( myClust, 1:nBoot, parBootAnyCovars, 
                                scaleData = scaleData, N = N ) 
  # save all bootstap data into matrix and output
  m <- matrix( unlist(tempbootResults), ncol = length(xDat), byrow=T)
  
  return( m ) 
}
# To compare the best function, just using R's boot
functionForUsingBoot <- function( scaleData, r)
{
  # sample data with replacement
  bootData <- matrix(unlist(scaleData[r,]), ncol=ncol(scaleData))
  #bootData <- scaleData[sample( 1:N, N, replace = T ),]
  Xmat <- bootData[,1:(ncol(scaleData)-1)]
  Ymat <- bootData[,ncol(scaleData)]
  # fit the model under this alternative reality
  # Changed the lm part to matrix form
  beta <- solve( t( Xmat ) %*% Xmat ) %*% t( Xmat ) %*% Ymat
  return(beta)
}
library(boot)
usingBootLibrary <- function( nBoot, yDat, ... ) 
{
  # parallelize
  library(parallel)
  nCores <- detectCores()
  myClust <- makeCluster(nCores-1, type = "PSOCK")
  
  # save x covariates into list
  xDat <- list(..., yDat)
  # create data matrix in one step
  scaleData <- cbind(1, do.call(cbind, xDat))
  
  N <- length( yDat ) 
  # pass scaled data to helper function to bootstrap
  tempbootResults <- boot(scaleData, functionForUsingBoot, R=nBoot)
  # save all bootstap data into matrix and output
  # this will return just the one, instead of a list of them
  return(matrix(unlist(tempbootResults$t0), ncol=length(xDat), byrow=T))
}

## In all functions above, i return a matrix, where the first column is the intercept
## and the rest of the columns are best estimate for each covar (in slope terms)
## To have final bootstrap results, take column means and calculate 95% CI.

## Testing for 1 as well as more x covariates
system.time( test3 <- bestBootCovars1( 100000, regData$y, regData$x) )
system.time( test4 <- bestBootCovars2( 100000, regData$y, regData$x) )
system.time( test5 <- bestBootCovars3( 100000, regData$y, regData$x) )
system.time( test6 <- oneBootToRuleThemAll( 100000, regData$y, regData$x) )
system.time( test7 <- usingBootLibrary( 100000, regData$y, regData$x) )

# works for factors
system.time( test8 <- oneBootToRuleThemAll( 100000, fitness$Oxygen, as.factor(fitness$Age)) )
# works for interaction terms
system.time( test9 <- oneBootToRuleThemAll( 100000, fitness$Oxygen, fitness$Age, fitness$Weight, fitness$Age*fitness$Weight) )

system.time( test10 <- bestBootCovars1( 100000, regData$y, regData$x, sampleData$Weight, sampleData$RestPulse, sampleData$RunPulse, sampleData$MaxPulse) )
system.time( test11 <- bestBootCovars2( 100000, regData$y, regData$x, sampleData$Weight, sampleData$RestPulse, sampleData$RunPulse, sampleData$MaxPulse) )
system.time( test12 <- oneBootToRuleThemAll( 100000, regData$y, regData$x, sampleData$Weight, sampleData$RestPulse, sampleData$RunPulse, sampleData$MaxPulse) )


# Compare efficient bootstrap to initial via microbenchmark
library(microbenchmark)
m <- summary(microbenchmark(lmBoot(regData, 1000), bestBootCovars1( 1000, regData$y, regData$x),
               bestBootCovars2( 1000, regData$y, regData$x), bestBootCovars3( 1000, regData$y, regData$x),
               usingBootLibrary( 1000, regData$y, regData$x), oneBootToRuleThemAll( 1000, regData$y, regData$x)))

#Plot it
m.df <- data.frame(m)
plotting <- data.frame(matrix(0, 6, 2))
plotting[, 2] <- m.df[["mean"]]
plotting[1,1] <- "lmBoot"
plotting[2,1] <- "bestBootCovars1"
plotting[3,1] <- "bestBootCovars2"
plotting[4,1] <- "bestBootCovars3"
plotting[5,1] <- "usingBootLibrary"
plotting[6,1] <- "oneBootToRuleThemAll"

minplotting <- data.frame(matrix(0, 6, 2))
minplotting[, 2] <- m.df[["lq"]]
minplotting[1,1] <- "lmBoot"
minplotting[2,1] <- "bestBootCovars1"
minplotting[3,1] <- "bestBootCovars2"
minplotting[4,1] <- "bestBootCovars3"
minplotting[5,1] <- "usingBootLibrary"
minplotting[6,1] <- "oneBootToRuleThemAll"

maxplotting <- data.frame(matrix(0, 6, 2))
maxplotting[, 2] <- m.df[["uq"]]
maxplotting[1,1] <- "lmBoot"
maxplotting[2,1] <- "bestBootCovars1"
maxplotting[3,1] <- "bestBootCovars2"
maxplotting[4,1] <- "bestBootCovars3"
maxplotting[5,1] <- "usingBootLibrary"
maxplotting[6,1] <- "oneBootToRuleThemAll"

ggplot( data = plotting, aes( x = X1, y = X2, fill = factor( X1 ))) +
  geom_bar( position = "dodge", stat = "identity" ) + 
  coord_flip() +
  scale_x_discrete(
    limits=c( "oneBootToRuleThemAll", 
              "usingBootLibrary","bestBootCovars3", "bestBootCovars2", 
              "bestBootCovars1", "lmBoot")) +
  ggtitle("Average time (in milliseconds) to run 1000 resampling iterations") +
  ylab("Time (ms)") + xlab("Function version") + theme(legend.position="none") 


