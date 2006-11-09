p <- function(s, ...){
  s <- sync(s)
  n <- length(s)
  par(mfrow = c(n, 1), mar = c(3, 4, 1, 1))
  for(i in 1:n){
    psac(s[i], axes = FALSE, ...)
    axis(side = 2)
    if(i == n) title(xlab = "Time"); axis(side = 1)
  }
}
