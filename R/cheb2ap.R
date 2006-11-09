cheb2ap <- function(n, rs){
  j <- complex(i=1)
  delta <- 1/sqrt(10^(0.1*rs)-1)
  mu <- asinh(1/delta)/n
  if(n %% 2 == 1){
    m <- n - 1
    z <- j / cos(c(seq(1,(n-2),by=2), seq(n+2,(2*n-1),by=2))*pi/(2*n))
  }else{
    m <- n
    z <- j / cos(seq(1, (2*n-1), by = 2)*pi/(2*n))
  }
  # Organize zeros in complex pairs:
  i <- rbind(1:(m/2) , seq(m, (m/2+1), by = -1))
  z <- z[i]
  p <- exp(j*(pi*seq(1, (2*n-1),by=2)/(2*n) + pi/2))
  p <- sinh(mu)*Re(p) + j*cosh(mu)*Im(p)
  p <- 1 / p
  k <- Re(prod(-p)/prod(-z))
  return(list(p = p, z = z, k = k))
}

