buildWindowMatrix <- function(dat, windowSize){
  #this function makes large "window" matrix
  # Example with 2 features and window size = 5
  #       |f1 f2 f3 f4 f5|F1 F2 F3 F4 F5|
  #       |f2 f3 f4 f5 f6|F2 F3 F4 F5 F6|
  #       |f3 f4 f5 f6 f7|F3 F4 F5 F6 F7|
  
  m = nrow(dat)
  n = ncol(dat)
  targetName = colnames(dat)[n]
  #take out y value column for cbind
  dat_window = matrix(0,ncol=1, nrow = m)
  colnames(dat_window) = targetName
  for(feature in 1:n){
    windowMatrix = matrix(0, ncol = windowSize, nrow = m)
    for(i in 1:windowSize){
      windowMatrix[(windowSize-i+1):m,i] = dat[1:(m-windowSize+i),feature]
    }
    targetName = colnames(dat)[feature]
    colnames(windowMatrix) = paste(as.character(targetName), windowSize:1, sep = "-")
    dat_window = cbind(dat_window, windowMatrix)
  }
  return(dat_window[ ,-1])
}