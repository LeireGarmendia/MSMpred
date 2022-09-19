# State the type of covariates depending on the number of unique values
covar_type <- function(covar,max_unique=5){
  cond1 <- is.numeric(covar)
  cond2 <- length(unique(covar))>max_unique
  
  if(cond1 & cond2){res <- "numeric"}else{res <- "factor"}
}

# Look for the rowname "x" in a mtrix tmat
search_rowname <- function(x,tmat) which(rownames(tmat)==x)

# Prepare data to fit the model
prepare_data <- function(data,tmat,time,status,covar,var_inistat="inistat"){
  print("3.1")
  datos <- msprep(data = data, trans = tmat,
                  time = time,
                  status =  status,
                  start = list(state = sapply(data[,var_inistat], 
                                              search_rowname, 
                                              tmat=tmat,
                                              simplify = TRUE), 
                               time = rep(0, nrow(data))),
                  keep = covar)
  print("3.2")
  datos <- expand.covs(datos, covar, longnames = FALSE)
}