rtrend <- function(s){
  for(i in 1:length(s)){
    x <- s[[i]]$x
    j <- 1:length(x)
    s[[i]]$x <- residuals(lm(x ~ j))
  }
  return(s)
}


