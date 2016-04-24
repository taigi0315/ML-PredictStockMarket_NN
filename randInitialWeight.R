randInitialWeight <- function(layer_in, layer_out){
  theta = matrix( rnorm((layer_in+1)*(layer_out),mean=0,sd=0.00001), layer_out,(layer_in+1))
  return(theta)
}
