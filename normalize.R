normalize <- function(x) {
  return ((x - mean(x)) / (max(x) - min(x)) )
}