chi_or_fisher <- function(df, var_1, var_2){
  table <- table(df[[var_1]], df[[var_2]])
  
  test <- tryCatch(
    chisq.test(table),
    error=function(e) e, 
    warning=function(w) w)
  if(is(test,"warning")){
    fisher = fisher.test(table,simulate.p.value = TRUE)
    fisher
  } else {
    chisq = chisq.test(table)
    chisq
  }
  
}