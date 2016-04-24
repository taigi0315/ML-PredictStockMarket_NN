#DataPrep_CommoData
buildCommoData = function(){
  Gold = read.csv("./Data/Gold_.csv")
  Silver = read.csv("./Data/Silver_.csv")
  Oil = read.csv("./Data/Oil_.csv")
  
  Apple <- read.csv(file = "./Data/Apple_.csv")
  CommoData = merge(Gold, Apple, all.y = TRUE, by="Date")
  CommoData = merge(Silver, CommoData,all.y = TRUE, by="Date")
  CommoData = merge(Oil, CommoData, all.y = TRUE, by="Date")
  CommoData = CommoData[ ,-ncol(CommoData)]
  CommoData = CommoData[order(as.Date(CommoData$Date, format="%m/%d/%Y")),]
  return (as.data.frame(CommoData))
}
