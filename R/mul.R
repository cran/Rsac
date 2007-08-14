mul <- function(s, c)
{
  n <- length(s)
  l <- length(c)
  if(l < n)
    c <- c(c, rep(c[l], (n - l)))
  for(i in 1:n)
    s[[i]]$x <- s[[i]]$x * c[i]
  return(s)
}

add <- function(s, c)
{
  n <- length(s)
  l <- length(c)
  if(l < n)
    c <- c(c, rep(c[l], (n - l)))
  for(i in 1:n)
    s[[i]]$x <- s[[i]]$x + c[i]
  return(s)
}

pow <- function(s, c)
{
  n <- length(s)
  l <- length(c)
  if(l < n)
    c <- c(c, rep(c[l], (n - l)))
  for(i in 1:n)
    s[[i]]$x <- s[[i]]$x ^ c[i]
  return(s)
}
