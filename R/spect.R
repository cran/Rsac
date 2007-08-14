spect <- function(s, log = "", ...)
{
  par(mfrow = c(length(s), 1), xaxs = "r", yaxs = "r")
  spect <- vector(mode = 'list', length = length(s))
  for(i in 1:length(s))
  {
    SPS <- 1/s[[i]]$delta
    x <- s[[i]]$x
    # Nyquist Frequency
    Fn <- SPS/2
    # Use next highest power of 2 greater than or equal to 
    # length(x) to calculate FFT
    NFFT <- nextn(length(x), 2)
    NumUniquePts <- ceiling((NFFT + 1)/2)
    # Zero-pad FFT to length(NFFT)
    Z <- array(0, NFFT)
    Z[1:length(x)] <- x
    x <- Z
    # Calculate FFT
    FFT <- fft(x)
    # FFT is symmetric, throw away second half
    FFTX <- FFT[1:NumUniquePts]
    #  Magnitude of FFT of x
    MX <- abs(FFTX)  
    # Scale the fft so that it is not a function of the
    # length of x
    MX <- MX/length(x)
    # Multiply by 2 to because you
    # threw out the second half of FFTX above
    MX <- MX*2
    # DC Component should be unique, thus not multiplied by 2 
    MX[1] <- MX[1]/2
    # If NFFT is odd, the Nyquist is not evaluated
    if( NFFT %% 2 == 0 )
      MX[length(MX)] <- MX[length(MX)]/2
    # This is an evenly spaced frequency vector with 
    # NumUniquePts points
    f <- ( 0 : (NumUniquePts - 1) ) * 2 * Fn/NFFT
    spect[[i]]$f <- f; spect[[i]]$MX <- MX
    # Generate the plot, title and labels.
    if(log != "")
      keep <- f > 0
    else
      keep <- rep(TRUE, length(f))
    f <- f[keep]; MX <- MX[keep]
    plot(f, MX, log = log, ...)
  }
  invisible(spect)
}
