lp <- function(s, c = 2, n = 1, p = 2, type = "BU", domain = "t"){
  if(p > 2 | p < 1) stop("not a supported value for p.\n")
  for(i in 1:length(s)){
    x <- s[[i]]$x; L <- length(x)
    SPS <- 1/s[[i]]$delta;
    fn <- SPS/2           # Nyquist Frequency
    NFFT <- nextn(length(x), 2)
    NumUniquePts <- ceiling((NFFT + 1)/2)
    Z <- array(0, NFFT)   # Zero-pad FFT to length(NFFT)
    Z[1:length(x)] <- x; x <- Z
    f <- seq(from = -fn, to = fn, length = NFFT)
    r <- c((NFFT/2 + 1):NFFT, 1:(NFFT/2))
    fs <- f[r]
    S <- complex(i = 1) * fs / c
    tfpz <- tf(type = type, n = n, plots = FALSE)
    H <- pz.tf(p = tfpz$p, z = tfpz$z, g = tfpz$k)
    if(domain == "t"){
      if(p == 1){
        tf <- fft(H(S))/NFFT
        fx <- convolve(x, tf)
        s[[i]]$x <- Re(fx)[1:L]
      }else{
        FFT <- fft(x)
        fFFT <- H(S) * FFT
        fFFT <- rev(rev(fFFT) * H(S))
        fx <- fft(fFFT, inverse = TRUE)/NFFT
        s[[i]]$x <- Re(fx)[1:L]
      }
    }else{
      if(p == 1){
        FFT <- fft(x)
        fFFT <- H(S) * FFT
        fx <- fft(fFFT, inverse = TRUE)/NFFT
        s[[i]]$x <- Re(fx)[1:L]
      }else{
        FFT <- fft(x)
        fFFT <- H(S) * FFT
        fFFT <- rev(rev(fFFT) * H(S))
        fx <- fft(fFFT, inverse = TRUE)/NFFT
        s[[i]]$x <- Re(fx)[1:L]
      }
    }
  }
  return(s)
}
