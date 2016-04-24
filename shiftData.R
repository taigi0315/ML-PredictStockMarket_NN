#this function shift data by 1 day.
#since it is trying to "predicting", we move y value by 1day 
#feature X for today - y value for tomorrow.

shiftData <- function(mainData){
  m = nrow(mainData)
  n = ncol(mainData)
  
  shifted_X = mainData[1:(m-1), 1:(n-1)]
  shifted_y = mainData[2:m, n]

  #prep normalized, cleaned(No NA), shifted(X : today's feature, y : tomorrow price ) data
  shiftedData = cbind(shifted_X, shifted_y)
  return(shiftedData)
}