predict <- function(theta1, theta2, X){
  m = nrow(X)
  
  h1 = sigmoid(cbind(rep(1,m),X) %*% t(theta1))
  h2 = sigmoid(cbind(rep(1,m),h1) %*% t(theta2))
  prediction = max.col(h2)
  return (prediction)
}