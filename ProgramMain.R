rm(list=ls())
#===== include header =====
source("header.R")
#===== data prep =====
#5window-transformed matrix
mainDataSet_normalize = as.data.frame(lapply(mainDataSet_original, normalize)) #normalize data
mainNormalWindow5 = buildWindowMatrix(mainDataSet_normalize, 5) #transform data with 5 width window
mainNormalWindow5 = shiftData(mainNormalWindow5)  #shift y value by 1 day(future prediction purpose)
dat = as.matrix(mainNormalWindow5)

#discretization y value(neg/pas or neg/neutral/pos)
yValue = dat[,ncol(dat)]
dat[yValue < 0.5, ncol(dat)] = 1
dat[yValue >= 0.5, ncol(dat)] = 2

#split train/test set
train = dat[1:700, ]
train_X = dat[1:700, 1:(ncol(dat)-1)]
train_y = dat[1:700, ncol(dat)]
test_X = dat[701:1001,1:(ncol(dat)-1) ]
test_y = dat[701:1001,ncol(dat)]

theta = NeuralNetworkMain(train, 25, 0.0001, 10000)
#NeuralNetworkMain returns list of theta, which contains theta1, theta2

#accuracy with train set
#prediction_train = predict(theta[[1]], theta[[2]], train_X)
#print(sum(prediction_train == train_y)/length(train_y))

#accuracy with test set
prediction= predict(theta[[1]], theta[[2]], test_X )
print(sum(prediction == test_y)/length(test_y))
