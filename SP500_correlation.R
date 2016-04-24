# ==== useful variable/file =====
SP500 <- read.csv(file = "SP500.csv" , header = TRUE)
m = nrow(SP500)
# ===== Grab S&P500 data from Quandl =====
for(i in 1 : m)
{
  code <- SP500[i, "free_code"]
  name <- SP500[i, "name"]
  
  catch_data <- Quandl(code = as.character(code), start_date="2012-01-01", 
                       end_date="2015-12-31", force_irregular = TRUE, transform = c("diff"))
  catch_data <- subset(catch_data , select = c(Date, Close))
  colnames(catch_data)[2] <- as.character(name)
  if(i == 1){
    data_table <- catch_data
  }
  else{
    data_table <- merge(catch_data, data_table, all = TRUE, by = "Date")
  }
}
data_table <- subset(data_table , select = -c(Date))
# ===== calculate z-score =====
zscore_table <- scale(data_table) 
zscore_cor <- data.frame(cor(zscore_table, use = "pairwise.complete.obs"), check.names=FALSE)
zscore_cor[is.na(zscore_cor)] <- 0
write.csv(zscore_cor, file = "zscore_cor_matrix.csv",
          quote = FALSE, row.names = TRUE, col.names = TRUE)
