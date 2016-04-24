#=====Neural Network Program =====
lambda = 0.05
num_labels = length(unique(FSD_windows[,ncol(FSD_windows)]))
input_layer_size = ncol(FSD_windows_month)-1
hidden_layer_size = round((input_layer_size+num_labels)*1/3,0)
max_iter = 5000
result_index = 5


#random initialize theta
theta1 = randInitialWeight(input_layer_size, hidden_layer_size)
theta2 = randInitialWeight(hidden_layer_size, num_labels)
initial_parameter = append(as.vector(theta1), as.vector(theta2))

#=====2002 -> 2003=====
theta_2002 = NeuralNetworkMain(FSD2002w, hidden_layer_size, lambda, max_iter, initial_parameter)
theta_2002_1 = theta_2002[[1]]
theta_2002_2 = theta_2002[[2]]
theta_2002 = append(theta_2002_1, theta_2002_2)

X = FSD2003w[, -ncol(FSD2015w)]
y = FSD2003w[ ,ncol(FSD2015w)]
prediction = predict(theta_2002_1, theta_2002_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2003 -> 2004=====
theta_2003 = NeuralNetworkMain(FSD2003w, hidden_layer_size, lambda, max_iter, theta_2002)
theta_2003_1 = theta_2003[[1]]
theta_2003_2 = theta_2003[[2]]
theta_2003 = append(theta_2003_1, theta_2003_2)

X = FSD2004w[, -ncol(FSD2015w)]
y = FSD2004w[ ,ncol(FSD2015w)]
prediction = predict(theta_2003_1, theta_2003_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2004 -> 2005=====
theta_2004 = NeuralNetworkMain(FSD2004w, hidden_layer_size, lambda, max_iter, theta_2003)
theta_2004_1 = theta_2004[[1]]
theta_2004_2 = theta_2004[[2]]
theta_2004 = append(theta_2004_1, theta_2004_2)

X = FSD2005w[, -ncol(FSD2015w)]
y = FSD2005w[ ,ncol(FSD2015w)]
prediction = predict(theta_2004_1, theta_2004_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2005 -> 2006=====
theta_2005 = NeuralNetworkMain(FSD2005w, hidden_layer_size, lambda, max_iter, theta_2004)
theta_2005_1 = theta_2005[[1]]
theta_2005_2 = theta_2005[[2]]
theta_2005 = append(theta_2005_1, theta_2005_2)

X = FSD2006w[, -ncol(FSD2015w)]
y = FSD2006w[ ,ncol(FSD2015w)]
prediction = predict(theta_2005_1, theta_2005_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2006 -> 2007=====
theta_2006 = NeuralNetworkMain(FSD2006w, hidden_layer_size, lambda, max_iter, theta_2005)
theta_2006_1 = theta_2006[[1]]
theta_2006_2 = theta_2006[[2]]
theta_2006 = append(theta_2006_1, theta_2006_2)

X = FSD2007w[, -ncol(FSD2015w)]
y = FSD2007w[ ,ncol(FSD2015w)]
prediction = predict(theta_2006_1, theta_2006_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2007 -> 2008 =====
theta_2007 = NeuralNetworkMain(FSD2007w, hidden_layer_size, lambda, max_iter, theta_2006)
theta_2007_1 = theta_2007[[1]]
theta_2007_2 = theta_2007[[2]]
theta_2007 = append(theta_2007_1, theta_2007_2)

X = FSD2008w[, -ncol(FSD2015w)]
y = FSD2008w[ ,ncol(FSD2015w)]
prediction = predict(theta_2007_1, theta_2007_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2008 -> 2009 =====
theta_2008 = NeuralNetworkMain(FSD2008w, hidden_layer_size, lambda, max_iter, theta_2007)
theta_2008_1 = theta_2008[[1]]
theta_2008_2 = theta_2008[[2]]
theta_2008 = append(theta_2008_1, theta_2008_2)

X = FSD2009w[, -ncol(FSD2015w)]
y = FSD2009w[ ,ncol(FSD2015w)]
prediction = predict(theta_2008_1, theta_2008_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2009 -> 2010=====
theta_2009 = NeuralNetworkMain(FSD2009w, hidden_layer_size, lambda, max_iter, theta_2008)
theta_2009_1 = theta_2009[[1]]
theta_2009_2 = theta_2009[[2]]
theta_2009 = append(theta_2009_1, theta_2009_2)

X = FSD2010w[, -ncol(FSD2015w)]
y = FSD2010w[ ,ncol(FSD2015w)]
prediction = predict(theta_2009_1, theta_2009_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2010 -> 2011=====
theta_2010 = NeuralNetworkMain(FSD2010w, hidden_layer_size, lambda, max_iter, theta_2009)
theta_2010_1 = theta_2010[[1]]
theta_2010_2 = theta_2010[[2]]
theta_2010 = append(theta_2010_1, theta_2010_2)

X = FSD2011w[, -ncol(FSD2015w)]
y = FSD2011w[ ,ncol(FSD2015w)]
prediction = predict(theta_2010_1, theta_2010_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2011 -> 2012=====
theta_2011 = NeuralNetworkMain(FSD2011w, hidden_layer_size, lambda, max_iter, theta_2010)
theta_2011_1 = theta_2011[[1]]
theta_2011_2 = theta_2011[[2]]
theta_2011 = append(theta_2011_1, theta_2011_2)

X = FSD2012w[, -ncol(FSD2015w)]
y = FSD2012w[ ,ncol(FSD2015w)]
prediction = predict(theta_2011_1, theta_2011_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2012 -> 2013=====
theta_2012 = NeuralNetworkMain(FSD2012w, hidden_layer_size, lambda, max_iter, theta_2011)
theta_2012_1 = theta_2012[[1]]
theta_2012_2 = theta_2012[[2]]
theta_2012 = append(theta_2012_1, theta_2012_2)

X = FSD2013w[, -ncol(FSD2015w)]
y = FSD2013w[ ,ncol(FSD2015w)]
prediction = predict(theta_2012_1, theta_2012_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2013 -> 2014=====
theta_2013 = NeuralNetworkMain(FSD2013w, hidden_layer_size, lambda, max_iter, theta_2012)
theta_2013_1 = theta_2013[[1]]
theta_2013_2 = theta_2013[[2]]
theta_2013 = append(theta_2013_1, theta_2013_2)

X = FSD2014w[, -ncol(FSD2015w)]
y = FSD2014w[ ,ncol(FSD2015w)]
prediction = predict(theta_2013_1, theta_2013_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2014 -> 2015=====
theta_2014 = NeuralNetworkMain(FSD2014w, hidden_layer_size, lambda, max_iter, theta_2013)
theta_2014_1 = theta_2014[[1]]
theta_2014_2 = theta_2014[[2]]
theta_2014 = append(theta_2014_1, theta_2014_2)

X = FSD2015w[, -ncol(FSD2015w)]
y = FSD2015w[ ,ncol(FSD2015w)]
prediction = predict(theta_2014_1, theta_2014_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1
#=====2015 -> 2016 =====
theta_2015 = NeuralNetworkMain(FSD2015w, hidden_layer_size, lambda, max_iter, theta_2014)
theta_2015_1 = theta_2015[[1]]
theta_2015_2 = theta_2015[[2]]
theta_2015 = append(theta_2015_1, theta_2015_2)

X = FSD2016w[, -ncol(FSD2015w)]
y = FSD2016w[ ,ncol(FSD2015w)]
prediction = predict(theta_2015_1, theta_2015_2, X)
result = sum(prediction == y)/length(y)
result_table[2, result_index] = result
result_index = result_index + 1

write.csv(result_table, file = "result_file.csv")

#=====training/test set =====
lambda = 1
num_labels = length(unique(training[,ncol(training)]))
input_layer_size = ncol(training)-1
hidden_layer_size = round((input_layer_size+num_labels)*1/3,0)
max_iter = 300

#random initialize theta
theta1 = randInitialWeight(input_layer_size, hidden_layer_size)
theta2 = randInitialWeight(hidden_layer_size, num_labels)
initial_parameter = append(as.vector(theta1), as.vector(theta2))

theta_training = NeuralNetworkMain(training, hidden_layer_size, lambda, max_iter, initial_parameter)
theta_training_1 = theta_training[[1]]
theta_training_2 = theta_training[[2]]
theta_training = append(theta_training_1, theta_training_2)


X_train = training[,-ncol(training)]
y_train = training[,ncol(training)]
predict_train = predict(theta_training_1, theta_training_2, X_train)
result = sum(predict_train == y_train)/ length(y_train)
result

X = test[ ,-ncol(test)]
y = test[ ,ncol(test)]
prediction = predict(theta_training_1, theta_training_2, X)
result = sum(prediction == y) / length(y)
result
