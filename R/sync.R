sync <- function(s){
  st <- sapply(s, fstart)
  st <- st - min(st)
  ref <- which(st == min(st))[1]
  reft <- list(nzhour = s[[ref]]$nzhour,
               nzmin = s[[ref]]$nzmin,
               nzsec = s[[ref]]$nzsec,
               nzmsec = s[[ref]]$nzmsec,
               b = s[[ref]]$b)
  for(i in 1:length(s)){
    s[[i]]$nzhour <- reft$nzhour
    s[[i]]$nzmin <- reft$nzmin
    s[[i]]$nzsec <- reft$nzsec
    s[[i]]$nzmsec <- reft$nzmsec
    s[[i]]$b <- s[[i]]$b + st[i]
    s[[i]]$e <- s[[i]]$e + st[i]
  }
  return(s)
}
