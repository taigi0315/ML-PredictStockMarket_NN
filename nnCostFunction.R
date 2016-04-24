nnCostFunction <- function(nn_parameter, input_layer_size, hidden_layer_size, num_labels, X, y, lambda){
  m = nrow(X)
  #split nn_parameter into theta1 and theta2
  theta1 = matrix(nn_parameter[1:(hidden_layer_size * (input_layer_size+1))], 
                  nrow = hidden_layer_size, ncol = (input_layer_size+1))
  theta2 = matrix(nn_parameter[(1+hidden_layer_size * (input_layer_size+1)):length(nn_parameter)],
                  nrow = num_labels, ncol = (hidden_layer_size+1))
  
  theta1_grad = matrix(0, nrow = nrow(theta1), ncol = ncol(theta1))
  theta2_grad = matrix(0, nrow = nrow(theta2), ncol = ncol(theta2))
  
  #build identity matrix using y value(column = y value is "1" & other columns are all "0")
  y_matrix = matrix(y, ncol = 1)  
  y_matrix = matrix(0, nrow = m, ncol = num_labels)
  for(i in 1:m){
    y_matrix[i,(y[i])] = 1;
  }
  
  a1 = cbind(rep(1,m),X)     #a1 = X with bias column
  z2 = a1 %*% t(theta1)     
  a2 = sigmoid(z2);          #the 'activation' output from neural layer. 
  a2 = cbind(rep(1,m), a2)
  z3 = a2 %*% t(theta2)
  a3 = sigmoid(z3);
  
  #compute non-regularzied cost
  left = (-y_matrix) * log(a3)
  right = (1-y_matrix) * log(1-a3)
  unregulCost_matrix = (1/m) * (left - right)
  unregulCost = sum(unregulCost_matrix)
  
  #compute regularzied component
  theta1NoBias = theta1[ ,2:ncol(theta1)]
  theta2NoBias = theta2[ ,2:ncol(theta2)]
  sumOfTheta1 = sum(theta1NoBias * theta1NoBias)
  sumOfTheta2 = sum(theta2NoBias * theta2NoBias)
  regulComp = (lambda/(2*m)) * (sumOfTheta1 + sumOfTheta2)
  regulCost = unregulCost + regulComp
  
  #backpropagation algorithm
  d3 = a3 - y_matrix
  d2 = (d3 %*% theta2NoBias) * sigmoidGradient(z2)
  Delta1 = t(d2) %*% a1
  Delta2 = t(d3) %*% a2
  
  #this is non-regularized theta gradient
  theta1_grad = Delta1 * (1/m)
  theta2_grad = Delta2 * (1/m)
  
  #set up first column of theta1 and theta2 for the condition that j != , = 0
  #(we do not multiply on bias column)
  theta1[ ,1] = 0;
  theta2[ ,1] = 0;
  
  #regularization component for theta
  theta1 = theta1 * (lambda/m)
  theta2 = theta2 * (lambda/m)
  theta1_grad = (Delta1 * (1/m)) + theta1
  theta2_grad = (Delta2 * (1/m)) + theta2
  
  theta_grad = append(as.vector(theta1_grad), as.vector(theta2_grad))
  return (theta_grad)
}
