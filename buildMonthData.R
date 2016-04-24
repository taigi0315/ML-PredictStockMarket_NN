buildMonthData = function(){
  Apple = read.csv("./Data/Apple_.csv")
  Apple = Apple[nrow(Apple):1, ]
  dateFormat = as.Date(Apple[,1], "%m/%d/%y")
  month = as.numeric(format.Date(dateFormat, "%m"))
  monthMatrix = matrix(0, ncol = 12, nrow = nrow(Apple))
  for (i in 1:nrow(monthMatrix)){
    monthMatrix[i,month[i]] = 1
  }
  colnames(monthMatrix) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  return(as.data.frame(monthMatrix))
}