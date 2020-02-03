
# 1 Modified the the R bootstrapping code to be more efficient. 
#   It is also be parallelised at some level. 
#   Profiled both the original and final versions as well as determining the overall speed increase. 
#   Included the profile file in my repository.
# 2 The new bootstrap function in R should be altered to accept an arbitrary number of covariates.
# 3 Modifications to the bootstrap function is version controlled via my project GitHub repository.
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

# function to pass boot
lmBoot <- function(inputData, nBoot) {
  
  for(i in 1:nBoot){
    
    # resample data with replacement
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

system.time(test <- lmBoot(regData, nBoot = 1000))

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

###############

# customer supporting function that will be used by parLapply function
# this fuction performs similar actions to Carls but without the for loop
parlmBoot <- function(index, scaleData, N)
{
  bootData <- scaleData[sample(1:N, N, replace = T),]
  xmat <- bootData[,1:2]
  ymat <- bootData[,3]
  
  # fit the model under this alternative reality
  # Changed the lm part to matrix form
  beta <- solve( t(xmat) %*% xmat) %*% t(xmat) %*% ymat
  bootResults <- beta
}

# final bootstrap function that utilises parLapply function to speed up process
parlmBoot1 <- function(inputData, nBoot)
{
  x <- cbind(1, scale( inputData$x, scale = F))
  y <- scale(inputData$y, scale = F)
  scaleData <- as.matrix(cbind( x, y))
  
  bootResults <- array(dim = c(nBoot, 2))
  
  N <- nrow(inputData)
  
  tempbootResults <- parLapply(myClust, 1:nBoot, parBadBoot, 
                                scaleData = scaleData, N = N) 
  
  bootResults <- tempbootResults 
  
  r <- bootResults[[1]]
  c <- bootResults[[2]]
  m <- matrix(c(r, c), ncol = 2, nrow = N)
  
  return(m) 
}

system.time(test2 <- parlmBoot1(inputData = regData, 10000))

################

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

system.time(lmBoot1(regData, nBoot = 1000))

##################


library('profvis')

profvis({
  data(fitness, package = "ggplot2")
  
  plot(Age ~ Oxygen, data = fitness)
  m <- lm(Age ~ Oxygen, data = fitness)
  abline(m, col = "red")
})

# using lapply

clusterEvalQ(myClust, library('profvis'))

library('foreach')

sumSeq <- function(reps) { sum( 1:reps) }
  anOutputList <- lapply(1:4, sumSeq)
  do.call('c', anOutputList)
  
  bootLM <- function(inputData, nBoot) {
    
    dataDim <- nrow(fitness)
    
    bootData <- data[sample(1:dataDim, dataDim, replace = T),]
        
    bootLM <- lm(y ~ x, data = bootData)
    
    coef(bootLM)
  
  }
  
system.time(sumSeq(reps = 10)) 



set.seed(180016660)
x <- fitness$Age
y <- fitness$Oxygen
contrivedData <- data.frame(x, y)

bootLM(1, contrivedData)

system.time(lmBoot1 <- foreach(n=1:100) %dopar% parlmBoot1(1000))



# Profiling
Rprof("~/Desktop/5763- A2/Software-Proj-2")








  






