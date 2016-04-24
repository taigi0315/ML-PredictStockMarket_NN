buildIndexData = function(){
  #Data Prep Code
  Apple <- read.csv(file = "./Data/Apple_.csv")
  ATX <- read.csv("./Data/ATX_.csv")
  CAC40 <- read.csv("./Data/CAC40_.csv")
  HangSeng <- read.csv("./Data/HangSeng_.csv")
  Kospi <- read.csv("./Data/Kospi_.csv")
  Nasdaq <- read.csv("./Data/Nasdaq_.csv")
  Nikkei225 <- read.csv("./Data/Nikkei225_.csv")
  Russell1000 <- read.csv("./Data/Russell1000_.csv")
  SP500 <- read.csv("./Data/SP500_.csv")
  STI <- read.csv("./Data/STI_.csv")
  SwissMarket <- read.csv("./Data/SwissMarket_.csv")
  Treasury5 <- read.csv("./Data/TreasuryYield5_.csv")
  Treasury30 <- read.csv("./Data/TreasuryYield30_.csv")
  
  IndexData = merge(ATX, Apple, all.y = TRUE)
  IndexData = merge(CAC40, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(HangSeng, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(Kospi, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(Nasdaq, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(Nikkei225, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(Russell1000, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(SP500, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(STI, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(SwissMarket, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(Treasury5, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = merge(Treasury30, IndexData, all.y = TRUE, by.y = "Date")
  IndexData = IndexData[ ,-ncol(IndexData)]
  
  IndexData = IndexData[order(as.Date(IndexData$Date, format="%m/%d/%Y")),]
  write.csv(IndexData, file = "IndexData.csv", row.names = FALSE)
  
  return (as.data.frame(IndexData))
}
