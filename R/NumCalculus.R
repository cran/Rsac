int <- function(s)
{
  for(i in 1:length(s))
  {
    dt <- s[[i]]$delta
    x <- s[[i]]$x
    A <- dt * x
    int <- cumsum(A)
    s[[i]]$x <- int
  }
  return(s)
}

dif <- function(s)
{
  for(i in 1:length(s))
  {
    x <- s[[i]]$x
    dt <- s[[i]]$delta
    s[[i]]$x <- diff(x)/dt
    s[[i]]$npts <- s[[i]]$npts - 1
    s[[i]]$b <- s[[i]]$b + 0.5 * dt
  }
  return(s)
}
