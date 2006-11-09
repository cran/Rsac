int <- function(s, domain = "f"){
  if(!(domain == "f" | domain == "t"))
    stop("Only accepted values of \'domain\' are \'f\' and \'t\'.")
#  s <- rtrend(s)
  if(domain == "t"){
    for(i in 1:length(s)){
      dt <- s[[i]]$delta
      x <- s[[i]]$x
      A <- dt * x
      int <- cumsum(A)
      s[[i]]$x <- int
    }
  }else{
    for(i in 1:length(s)){
      x <- s[[i]]$x; L <- length(x)
      SPS <- 1/s[[i]]$delta
      fn <- SPS/2
      NFFT <- nextn(length(x), 2)
      NumUniquePts <- ceiling((NFFT + 1)/2)
      Z <- array(0, NFFT)
      Z[1:length(x)] <- x; x <- Z
      FFT <- fft(x)
      f <- seq(from = -fn, to = fn, length = NFFT)
      fs <- f[ c((NFFT/2 + 1):NFFT, 1:(NFFT/2))]
      S <- complex(i=1) * fs
      fFFT <- FFT / (2*pi*S)
      fx <- fft(fFFT, inverse = TRUE)/NFFT
      s[[i]]$x <- Re(fx)[1:L]
      s <- pztransfer(s = s, p = NULL, z = 0, g = 2*pi, domain = "f")
    }
  }
  return(s)
}
