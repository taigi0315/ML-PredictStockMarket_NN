myQuandl <- function(code, name){
  target = Quandl(code = as.character(code), start_date = "2012-01-01", end_date = "2015-12-31", transform = "diff", order = "asc")
  return (target)
}