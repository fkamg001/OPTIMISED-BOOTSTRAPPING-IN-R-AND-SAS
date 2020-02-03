### really not what we need but works
# bestBadBootBrother_allCovariates <- function( nBoot, ... ){
#   dat <- list(...)
#   print(nBoot)
#   x <- cbind( 1, scale( dat[[1]], scale = F ) )
#   y <- scale( dat[[2]], scale = F )
#   scaleData <- as.matrix( cbind( x, y ) )
#   
#   bootResults <- array( dim = c( nBoot, 2 ) )
#   
#   N <- nrow( length(dat) )
#   
#   tempbootResults <- parLapply( myClust, 1:nBoot, parBadBoot, 
#                                 scaleData = scaleData, N = N ) 
#   
#   bootResults <- tempbootResults 
#   r <- bootResults[[1]]
#   c <- bootResults[[2]]
#   m <- matrix( c( r, c ), ncol = 2, nrow = N )
#   
#   return( m ) 
# }  
# system.time( test2 <- somecode( 100000, regData$x, regData$y ) )

# so could use solve (crossprod) instead but matrix mult faster
parBadBoot <- function( index, scaleData, N )
{
  bootData <- scaleData[sample( 1:N, N, replace = T ),]
  Xmat <- bootData[,1:2]
  Ymat <- bootData[,3]
  
  # fit the model under this alternative reality
  # Changed the lm part to matrix form
  #beta <- solve( t( Xmat ) %*% Xmat ) %*% t( Xmat ) %*% Ymat
  beta <- solve(crossprod(Xmat), crossprod(Xmat,Ymat))
  bootResults <- beta
}


parBadBootAnyCovars <- function( index, scaleData, N )
{
  bootData <- scaleData[sample( 1:N, N, replace = T ),]
  Xmat <- bootData[,1:(ncol(bootData)-1)]
  Ymat <- bootData[,ncol(bootData)]
  
  # fit the model under this alternative reality
  # Changed the lm part to matrix form
  beta <- solve( t( Xmat ) %*% Xmat ) %*% t( Xmat ) %*% Ymat
  bootResults <- beta
}

bestBadBootBrotherAnyCovars <- function( nBoot, yDat, ... ) 
{
  xDat <- list(...)
  x <- cbind(1, scale(xDat[[1]], scale = F))
  if(length(xDat) > 1) {
     for(i in 2:length(xDat)) {
         x <- cbind(x, scale(xDat[[i]], scale = F))
     }
  }
  y <- scale( yDat, scale = F )
  scaleData <- as.matrix( cbind( x, y ) )
  bootResults <- array( dim = c( nBoot, 2 ) )
  
  N <- length( yDat ) 

  tempbootResults <- parLapply( myClust, 1:nBoot, parBadBoot, 
                                scaleData = scaleData, N = N ) 
  
  bootResults <- tempbootResults 
  
  r <- bootResults[[1]]
  c <- bootResults[[2]]
  m <- matrix( c( r, c ), ncol = 2, nrow = N )
  
  return( m ) 
}

system.time( test2 <- bestBadBootBrotherAnyCovars( 100, regData$y, regData$x ) )

microbenchmark(lmBoot(regData, 100), bestBadBootBrotherAnyCovars( 100, regData$y, regData$x))
