#==require packages==
#install.packages("devtools")
#install_github('quandl/R-package')

#==Neural Network Implementation Sources==
source("randInitialWeight.R") 
source("sigmoid.R")
source("sigmoidGradient.R")
source("nnCostFunction.R")
source("predict.R")
source("NeuralNetworkMain.R")

#==Data handling Implementation Sources==
source("myQuandl.R")
source("normalize.R")
source("buildwindowMatrix.R")
source("shiftData.R")
source("buildCurrencyData.R")
source("buildCommoData.R")
source("buildIndexData.R")
source("buildLinearMatrix.R")
source("buildMonthData.R")

#library file
library(Quandl)
library(devtools)
library(zoo)

#Quandl Auth
Quandl.api_key("b8FKnSAiCQ7okzdFsqgz")
