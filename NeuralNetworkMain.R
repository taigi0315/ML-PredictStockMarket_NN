NeuralNetworkMain <- function(dat, hidden_layer_size, lambda, iteration, initial_theta){
  #variable set up
  m = nrow(dat)
  n = ncol(dat)
  y = dat[ ,n]
  X = dat[ ,1:(n-1)]
  num_labels = length(unique(y))
  input_layer_size = n-1
  #training
  for(iter in 1:iteration){
    theta_grad = nnCostFunction(initial_parameter, input_layer_size, 
                                hidden_layer_size, num_labels, X, y, lambda)
    #split the result of theta_grad into theta1 and theta2
    theta1_grad = matrix(theta_grad[1:(hidden_layer_size * (input_layer_size+1))], 
                         nrow = hidden_layer_size, ncol = (input_layer_size+1))
    theta2_grad = matrix(theta_grad[(1+hidden_layer_size * (input_layer_size+1)):length(theta_grad)],
                         nrow = num_labels, ncol = (hidden_layer_size+1))
    theta1 = theta1 - theta1_grad
    theta2 = theta2 - theta2_grad
    next_theta = append(as.vector(theta1), as.vector(theta2))
    
    initial_parameter = next_theta
  }
  theta = list(theta1, theta2)
  return(theta)
}
