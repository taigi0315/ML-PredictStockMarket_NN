#=====building data main=====
#read y value and discretize 
y = read.csv("./Data/Apple_Diff_.csv")
y = y[nrow(y):1, ]

#build datas
CommoData = buildCommoData()
CommoData = lapply(CommoData, na.locf)
CommoData = as.data.frame(CommoData)
IndexData = buildIndexData()
IndexData = lapply(IndexData, na.locf)
IndexData = as.data.frame(IndexData)
CurData = buildCurrencyData()
CurData = lapply(CurData, na.locf)
CurData = as.data.frame(CurData)
MonthData = buildMonthData()
correlationData = read.csv(file = "zscore_cor_matrix.csv")
UnionPacific = read.csv(file = "./Data/UnionPacific_.csv")
UnionPacific = UnionPacific[nrow(UnionPacific):1, ]

#combine datas all together, take out "Date" column
combinedData = cbind(UnionPacific[,-1],CommoData[,-1], IndexData[,-1],CurData[,-1],Target = y[,-1])

linear3 = buildLinearMatrix(combinedData, 3)
linear5 = buildLinearMatrix(combinedData, 5)
linear10 = buildLinearMatrix(combinedData, 10)
linear30 = buildLinearMatrix(combinedData, 30)
linear60 = buildLinearMatrix(combinedData, 60)
#cut first sample to make the same size with other data
linear3 = linear3[-1, ]
linear5 = linear5[-1, ]
linear10 = linear10[-1, ]
linear30 = linear30[-1, ]
linear60 = linear60[-1, ]
#make a new "difference" data table using combined data
diff_combinedData = apply(combinedData[nrow(combinedData):1, ], 2, diff)
diff_combinedData = -diff_combinedData[nrow(diff_combinedData):1, ]

#build windows data, pass parameter size of window
windowSize = 3
fullStackData = diff_combinedData
FSD_windows = buildWindowMatrix(fullStackData, windowSize)
FSD_windows = cbind(linear3, linear5, linear10, linear30, linear60, FSD_windows)

#changing Target column's column name
colnames(FSD_windows)[ncol(FSD_windows)] = "Target"
FSD_windows_month = cbind(MonthData[-1, ], FSD_windows)



#take out first "windowSize" columns because its zeros
max_windowSize = 10
FSD_windows = FSD_windows[-c(1:max_windowSize), ]
FSD_windows_month = as.matrix(FSD_windows_month[-c(1:(max_windowSize-1)), ])

#discretize Target column
positive = FSD_windows[ ,ncol(FSD_windows)]>0
negative = FSD_windows[ ,ncol(FSD_windows)]<=0
FSD_windows[positive, ncol(FSD_windows)] = 2
FSD_windows[negative, ncol(FSD_windows)] = 1
#discretize Target column -with month data
positive = FSD_windows_month[ ,ncol(FSD_windows_month)]>0
negative = FSD_windows_month[ ,ncol(FSD_windows_month)]<=0
FSD_windows_month[positive, ncol(FSD_windows_month)] = 2
FSD_windows_month[negative, ncol(FSD_windows_month)] = 1

#split the data for year term learning
FSD2002 = FSD_windows[1:(250-max_windowSize), ]
FSD2003 = FSD_windows[(251-max_windowSize):(502-max_windowSize), ]
FSD2004 = FSD_windows[(503-max_windowSize):(754-max_windowSize), ]
FSD2005 = FSD_windows[(755-max_windowSize):(1006-max_windowSize), ]
FSD2006 = FSD_windows[(1007-max_windowSize):(1257-max_windowSize), ]
FSD2007 = FSD_windows[(1258-max_windowSize):(1508-max_windowSize), ]
FSD2008 = FSD_windows[(1509--max_windowSize):(1761-max_windowSize), ]
FSD2009 = FSD_windows[(1762-max_windowSize):(2013-max_windowSize), ]
FSD2010 = FSD_windows[(2014-max_windowSize):(2265-max_windowSize), ]
FSD2011 = FSD_windows[(2266-max_windowSize):(2517-max_windowSize), ]
FSD2012 = FSD_windows[(2518-max_windowSize):(2767-max_windowSize), ]
FSD2013 = FSD_windows[(2768-max_windowSize):(3019-max_windowSize), ]
FSD2014 = FSD_windows[(3020-max_windowSize):(3271-max_windowSize), ]
FSD2015 = FSD_windows[(3272-max_windowSize):(3523-max_windowSize), ]
FSD2016 = FSD_windows[(3524-max_windowSize):nrow(FSD_windows), ]

#split the data for year term learning , with month data
FSD2002w = as.matrix(FSD_windows_month[1:(250-max_windowSize), ])
FSD2003w = as.matrix(FSD_windows_month[(251-max_windowSize):(502-max_windowSize), ])
FSD2004w = as.matrix(FSD_windows_month[(503-max_windowSize):(754-max_windowSize), ])
FSD2005w = as.matrix(FSD_windows_month[(755-max_windowSize):(1006-max_windowSize), ])
FSD2006w = as.matrix(FSD_windows_month[(1007-max_windowSize):(1257-max_windowSize), ])
FSD2007w = as.matrix(FSD_windows_month[(1258-max_windowSize):(1508-max_windowSize), ])
FSD2008w = as.matrix(FSD_windows_month[(1509--max_windowSize):(1761-max_windowSize), ])
FSD2009w = as.matrix(FSD_windows_month[(1762-max_windowSize):(2013-max_windowSize), ])
FSD2010w = as.matrix(FSD_windows_month[(2014-max_windowSize):(2265-max_windowSize), ])
FSD2011w = as.matrix(FSD_windows_month[(2266-max_windowSize):(2517-max_windowSize), ])
FSD2012w = as.matrix(FSD_windows_month[(2518-max_windowSize):(2767-max_windowSize), ])
FSD2013w = as.matrix(FSD_windows_month[(2768-max_windowSize):(3019-max_windowSize), ])
FSD2014w = as.matrix(FSD_windows_month[(3020-max_windowSize):(3271-max_windowSize), ])
FSD2015w = as.matrix(FSD_windows_month[(3272-max_windowSize):(3523-max_windowSize), ])
FSD2016w = as.matrix(FSD_windows_month[(3524-max_windowSize):nrow(FSD_windows_month), ])

training = FSD_windows_month[1:3090, ]
test = FSD_windows_month[3091:nrow(FSD_windows), ]

training2 = FSD_windows[1:3090, ]
test2 = FSD_windows[3091:nrow(FSD_windows), ]

