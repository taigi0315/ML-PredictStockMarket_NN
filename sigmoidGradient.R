sigmoidGradient <- function(z){
  if(class(z) == "data.frame"){
    m = nrow(z)  
  }
  else
    m = length(z)
  a = sigmoid(z)
  g = a * (1-a)
  return(g)
}