#header include all program source
#buildDataMain has all data prep part source
rm(list=ls())
source("header.R")
source("buildDataMain.R")

#=====Neural Network Program =====
lambda = 10
num_labels = length(unique(FSD_windows[,ncol(FSD_windows)]))
input_layer_size = ncol(FSD_windows)-1
hidden_layer_size = 15
max_iter = 10000

#save result in table
result_table = matrix(0, ncol = 18, nrow = 2)
colnames(result_table) = c("w Month", "lambda", "#hidden_node", "iter", "acc_2003", "acc_2004", "acc_2005", "acc_2006", "acc_2007"
                           , "acc_2008", "acc_2009", "acc_2010", "acc_2011", "acc_2012", "acc_2013", "acc_2014", "acc_2015", "acc_2016")
result_table[1:2,1] = c(0,1)
result_table[1, 2:4] = c(lambda, hidden_layer_size, max_iter)
result_table[2, 2:4] = c(lambda, hidden_layer_size, max_iter)
result_index = 5

#random initialize theta
theta1 = randInitialWeight(input_layer_size, hidden_layer_size)
theta2 = randInitialWeight(hidden_layer_size, num_labels)
initial_parameter = append(as.vector(theta1), as.vector(theta2))

#=====2002 -> 2003=====
theta_2002 = NeuralNetworkMain(FSD2002, hidden_layer_size, lambda, max_iter, initial_parameter)
theta_2002_1 = theta_2002[[1]]
theta_2002_2 = theta_2002[[2]]
theta_2002 = append(theta_2002_1, theta_2002_2)

X = FSD2003[, -ncol(FSD2015)]
y = FSD2003[ ,ncol(FSD2015)]
prediction = predict(theta_2002_1, theta_2002_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2003 -> 2004=====
theta_2003 = NeuralNetworkMain(FSD2003, hidden_layer_size, lambda, max_iter, theta_2002)
theta_2003_1 = theta_2003[[1]]
theta_2003_2 = theta_2003[[2]]
theta_2003 = append(theta_2003_1, theta_2003_2)

X = FSD2004[, -ncol(FSD2015)]
y = FSD2004[ ,ncol(FSD2015)]
prediction = predict(theta_2003_1, theta_2003_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2004 -> 2005=====
theta_2004 = NeuralNetworkMain(FSD2004, hidden_layer_size, lambda, max_iter, theta_2003)
theta_2004_1 = theta_2004[[1]]
theta_2004_2 = theta_2004[[2]]
theta_2004 = append(theta_2004_1, theta_2004_2)

X = FSD2005[, -ncol(FSD2015)]
y = FSD2005[ ,ncol(FSD2015)]
prediction = predict(theta_2004_1, theta_2004_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2005 -> 2006=====
theta_2005 = NeuralNetworkMain(FSD2005, hidden_layer_size, lambda, max_iter, theta_2004)
theta_2005_1 = theta_2005[[1]]
theta_2005_2 = theta_2005[[2]]
theta_2005 = append(theta_2005_1, theta_2005_2)

X = FSD2006[, -ncol(FSD2015)]
y = FSD2006[ ,ncol(FSD2015)]
prediction = predict(theta_2005_1, theta_2005_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2006 -> 2007=====
theta_2006 = NeuralNetworkMain(FSD2006, hidden_layer_size, lambda, max_iter, theta_2005)
theta_2006_1 = theta_2006[[1]]
theta_2006_2 = theta_2006[[2]]
theta_2006 = append(theta_2006_1, theta_2006_2)

X = FSD2007[, -ncol(FSD2015)]
y = FSD2007[ ,ncol(FSD2015)]
prediction = predict(theta_2006_1, theta_2006_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2007 -> 2008 =====
theta_2007 = NeuralNetworkMain(FSD2007, hidden_layer_size, lambda, max_iter, theta_2006)
theta_2007_1 = theta_2007[[1]]
theta_2007_2 = theta_2007[[2]]
theta_2007 = append(theta_2007_1, theta_2007_2)

X = FSD2008[, -ncol(FSD2015)]
y = FSD2008[ ,ncol(FSD2015)]
prediction = predict(theta_2007_1, theta_2007_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2008 -> 2009 =====
theta_2008 = NeuralNetworkMain(FSD2008, hidden_layer_size, lambda, max_iter, theta_2007)
theta_2008_1 = theta_2008[[1]]
theta_2008_2 = theta_2008[[2]]
theta_2008 = append(theta_2008_1, theta_2008_2)

X = FSD2009[, -ncol(FSD2015)]
y = FSD2009[ ,ncol(FSD2015)]
prediction = predict(theta_2008_1, theta_2008_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2009 -> 2010=====
theta_2009 = NeuralNetworkMain(FSD2009, hidden_layer_size, lambda, max_iter, theta_2008)
theta_2009_1 = theta_2009[[1]]
theta_2009_2 = theta_2009[[2]]
theta_2009 = append(theta_2009_1, theta_2009_2)

X = FSD2010[, -ncol(FSD2015)]
y = FSD2010[ ,ncol(FSD2015)]
prediction = predict(theta_2009_1, theta_2009_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2010 -> 2011=====
theta_2010 = NeuralNetworkMain(FSD2010, hidden_layer_size, lambda, max_iter, theta_2009)
theta_2010_1 = theta_2010[[1]]
theta_2010_2 = theta_2010[[2]]
theta_2010 = append(theta_2010_1, theta_2010_2)

X = FSD2011[, -ncol(FSD2015)]
y = FSD2011[ ,ncol(FSD2015)]
prediction = predict(theta_2010_1, theta_2010_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2011 -> 2012=====
theta_2011 = NeuralNetworkMain(FSD2011, hidden_layer_size, lambda, max_iter, theta_2010)
theta_2011_1 = theta_2011[[1]]
theta_2011_2 = theta_2011[[2]]
theta_2011 = append(theta_2011_1, theta_2011_2)

X = FSD2012[, -ncol(FSD2015)]
y = FSD2012[ ,ncol(FSD2015)]
prediction = predict(theta_2011_1, theta_2011_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2012 -> 2013=====
theta_2012 = NeuralNetworkMain(FSD2012, hidden_layer_size, lambda, max_iter, theta_2011)
theta_2012_1 = theta_2012[[1]]
theta_2012_2 = theta_2012[[2]]
theta_2012 = append(theta_2012_1, theta_2012_2)

X = FSD2013[, -ncol(FSD2015)]
y = FSD2013[ ,ncol(FSD2015)]
prediction = predict(theta_2012_1, theta_2012_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2013 -> 2014=====
theta_2013 = NeuralNetworkMain(FSD2013, hidden_layer_size, lambda, max_iter, theta_2012)
theta_2013_1 = theta_2013[[1]]
theta_2013_2 = theta_2013[[2]]
theta_2013 = append(theta_2013_1, theta_2013_2)

X = FSD2014[, -ncol(FSD2015)]
y = FSD2014[ ,ncol(FSD2015)]
prediction = predict(theta_2013_1, theta_2013_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2014 -> 2015=====
theta_2014 = NeuralNetworkMain(FSD2014, hidden_layer_size, lambda, max_iter, theta_2013)
theta_2014_1 = theta_2014[[1]]
theta_2014_2 = theta_2014[[2]]
theta_2014 = append(theta_2014_1, theta_2014_2)

X = FSD2015[, -ncol(FSD2015)]
y = FSD2015[ ,ncol(FSD2015)]
prediction = predict(theta_2014_1, theta_2014_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1
#=====2015 -> 2016 =====
theta_2015 = NeuralNetworkMain(FSD2015, hidden_layer_size, lambda, max_iter, theta_2014)
theta_2015_1 = theta_2015[[1]]
theta_2015_2 = theta_2015[[2]]
theta_2015 = append(theta_2015_1, theta_2015_2)

X = FSD2016[, -ncol(FSD2015)]
y = FSD2016[ ,ncol(FSD2015)]
prediction = predict(theta_2015_1, theta_2015_2, X)
result = sum(prediction == y)/length(y)
result_table[1, result_index] = result
result_index = result_index + 1







