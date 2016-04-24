#build currency data
buildCurrencyData = function(){
  Apple = read.csv("./Data/Apple_.csv")
  CUR.CNY = read.csv("./Data/CUR-CNY_.csv")
  CUR.COP = read.csv("./Data/CUR-COP_.csv")
  CUR.EUR = read.csv("./Data/CUR-EUR_.csv")
  CUR.JPY = read.csv("./Data/CUR-JPY_.csv")
  CUR.KRW = read.csv("./Data/CUR-KRW_.csv")
  CUR.MYR = read.csv("./Data/CUR-MYR_.csv")
  CUR.THB = read.csv("./Data/CUR-THB_.csv")
  
  CurData = merge(CUR.CNY, Apple, all.y = TRUE, by = "Date")
  CurData = merge(CUR.COP, CurData, all.y = TRUE, by = "Date")
  CurData = merge(CUR.EUR, CurData, all.y = TRUE, by = "Date")
  CurData = merge(CUR.JPY, CurData, all.y = TRUE, by = "Date")
  CurData = merge(CUR.KRW, CurData, all.y = TRUE, by = "Date")
  CurData = merge(CUR.MYR, CurData, all.y = TRUE, by = "Date")
  CurData = merge(CUR.THB, CurData, all.y = TRUE, by = "Date")
  
  CurData = CurData[ ,-ncol(CurData)]
  CurData = CurData[order(as.Date(CurData$Date, format="%m/%d/%Y")),]
  return (as.data.frame(CurData))
}
