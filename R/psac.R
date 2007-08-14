psac <- function(s, ylim = NULL, lty = NULL, col = NULL, ...)
{
  n <- length(s)
  maxes <- sapply(s, maxx)
  mins <- sapply(s, minx)
  st <- sapply(s, fstart)
  st <- st - min(st)
  if(is.null(col))
    col <- rep((1:6), ceiling(n/6))[1:n]
  if(is.null(lty))
    lty <- gl(length = n, n = ceiling(n/6), k = 6)
  if(is.null(ylim))
    ylim <- c(min(mins), max(maxes))
  for(i in 1:n)
  {
    x <- s[[i]]$x
    start <- st[i] + s[[i]]$b; deltat <- s[[i]]$delta
    time <- seq(from = start, by = deltat, length = length(x))
    if(i == 1 ) plot(time, x, type = "l", ylim = ylim, ...)
    else lines(time, x, lty = as.numeric(lty[i]), col = col[i])
  }
}

maxx <- function(X) max(X$x)
minx <- function(X) min(X$x)
fstart <- function(X) X$nzhour*60*60+X$nzmin*60+X$nzsec+X$nzmsec*1e-3




  
