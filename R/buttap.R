buttap <- function(n){
  z <- NULL
  if(n > 1){
    p <- exp(complex(i = 1)*(pi*seq(1, (n - 1), by = 2)/(2*n) + pi/2))
    p <- c(p, Conj(p))
    #  p <- p(:) # I don't think this actually does anything....
    # n is odd
    if(n%%2 == 1)  p <- c(p, -1)
    k <- Re(prod(-p))
  }else{
    p <- 1; k <- 1
  }
  return(list(p = p, k = k, z = z))
}
