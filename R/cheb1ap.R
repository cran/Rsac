cheb1ap <- function(n, rp){
  j <- complex(i = 1)
  epsilon <- sqrt(10^(0.1 * rp) - 1)
  mu <- asinh(1/epsilon)/n
  p <- exp(j*(pi*seq(1, (2*n-1), by = 2)/(2*n) + pi/2))
  p <- sinh(mu) * Re(p) + j * cosh(mu) * Im(p)
  z <- NULL 
  k <- Re(prod(-p))
      # n is even so patch k 
  if(n %% 2 == 0) k <- k/sqrt((1 + epsilon^2))
  return(list(p = p, z = z, k = k))
}
