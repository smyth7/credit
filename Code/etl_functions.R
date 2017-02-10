# ETL and basic analysis functions

numUnique<- function(x){
  
  x %>% unique() %>% length()
  
}

numNAs<- function(x){
  
  x %>% is.na() %>% sum()
  
}

propNAs<- function(x){
  
  nr<- length(x)
  y<- x %>% is.na() %>% sum()
  return(y/nr)
  
}