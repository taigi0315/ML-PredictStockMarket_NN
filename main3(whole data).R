#header include all program source
#buildDataMain has all data prep part source
rm(list=ls())
source("header.R")
source("buildDataMain.R")

#=====training/test set =====
lambda = 10
num_labels = length(unique(training[,ncol(training)]))
input_layer_size = ncol(training)-1
hidden_layer_size = 50
max_iter = 800

#random initialize theta
theta1 = randInitialWeight(input_layer_size, hidden_layer_size)
theta2 = randInitialWeight(hidden_layer_size, num_labels)
initial_parameter = append(as.vector(theta1), as.vector(theta2))

#whole data with month
theta_training = NeuralNetworkMain(training, hidden_layer_size, lambda, max_iter, initial_parameter)
theta_training_1 = theta_training[[1]]
theta_training_2 = theta_training[[2]]
theta_training = append(theta_training_1, theta_training_2)

X_train = training[,-ncol(training)]
y_train = training[,ncol(training)]
predict_train = predict(theta_training_1, theta_training_2, X_train)
result_train = sum(predict_train == y_train)/ length(y_train)
print(result_train)

X = test[ ,-ncol(test)]
y = test[ ,ncol(test)]
prediction = predict(theta_training_1, theta_training_2, X)
result_test = sum(prediction == y) / length(y)
print(result_test)

#==== FSD with month column running =====

#whole data without month
num_labels = length(unique(training2[,ncol(training2)]))
input_layer_size = ncol(training2)-1
hidden_layer_size = 10

#random initialize theta
theta1 = randInitialWeight(input_layer_size, hidden_layer_size)
theta2 = randInitialWeight(hidden_layer_size, num_labels)
initial_parameter = append(as.vector(theta1), as.vector(theta2))

theta_training = NeuralNetworkMain(training2, hidden_layer_size, lambda, max_iter, initial_parameter)
theta_training_1 = theta_training[[1]]
theta_training_2 = theta_training[[2]]
theta_training = append(theta_training_1, theta_training_2)

X_train = training2[,-ncol(training2)]
y_train = training2[,ncol(training2)]
predict_train = predict(theta_training_1, theta_training_2, X_train)
result_train = sum(predict_train == y_train)/ length(y_train)
print(result_train)

X = test2[ ,-ncol(test2)]
y = test2[ ,ncol(test2)]
prediction = predict(theta_training_1, theta_training_2, X)
result_test = sum(prediction == y) / length(y)
print(result_test)

