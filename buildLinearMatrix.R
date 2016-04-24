buildLinearMatrix = function(dat, windowSize){
  #getLinearSlope takes the matrix of x values and y values
  #returns a slope of it using linear regression(normal equation)
  getLinearSlope <- function(x,y){
    x=cbind(matrix(1,nrow=length(x),ncol=1), x)
    a=solve(t(x) %*% x) %*% (  t(x) %*% y ) 
    return (a[2])
  }
  
  #getLinearColumn takes whole column of data, its y value and size of window
  #returns vector of slope per window
  getLinearColumn <- function(dat, windowSize){
    slopeMatrix = matrix(0, nrow=(nrow(dat)-windowSize+1), ncol=1)
    for(rIndex in windowSize:nrow(dat)){
      thisY = dat[(rIndex-windowSize+1):rIndex,1]
      thisX = c(1:windowSize)
      slope = getLinearSlope(thisX, thisY)
      slopeMatrix[rIndex] = slope
    }
    return (slopeMatrix)
  }
  
  #getLinearMatrix gets data, windowSize
  #returns matrix of each column's linear slope
  getLinearMatrix <- function(dat, windowSize){
    linearMatrix = matrix(0, nrow=nrow(dat), ncol=(ncol(dat)-1) )
    for(cIndex in 1 : (ncol(dat)-1) ){
      thisDat = dat[ ,c(cIndex, ncol(dat))]
      thisLinearColumn = getLinearColumn(thisDat, windowSize)
      linearMatrix[ ,cIndex] = thisLinearColumn
    }
    return (linearMatrix)
  }
  result = getLinearMatrix(dat, windowSize)
  colnames(result) = paste(paste(colnames(dat)[1:(ncol(dat)-1)],"linear"),windowSize)
  return (result)
}
