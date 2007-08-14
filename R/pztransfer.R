pztransfer <- function(s, file = NULL, p = NULL, z = NULL, g = NULL,
                       domain = "t", decon = FALSE)
{
  if(!(domain == "f" | domain == "t"))
    stop("Only accepted values of \'domain\' are \'f\' and \'t\'.")
  if(decon == TRUE & domain == "t")
    stop("Deconvolution is only supported in the frequency domain.")
  if(all(c(is.null(file), is.null(p), is.null(z))))
     stop("Please provide either poles and zeros or the path to a file.")
  if(!all(is.null(p), is.null(z)) & !is.null(file))
    stop("\'file\' and p/z are specified. Please provide only one or the other.")
  if(!is.null(file))
  {
    # - - Read in the p, z, and g - - #
    dat <- scan(file = file, what = character())
    fz <- grep("ZEROS", dat)
    fp <- grep("POLES", dat)
    fg <- grep("CONSTANT", dat)
    nz <- as.numeric(dat[fz + 1]); nz2 <- (fp - fz -2)/2
    np <- as.numeric(dat[fp + 1]); np2 <- (fg - fp -2)/2
    g <- as.numeric(dat[fg + 1])
    z <- NULL; p <- NULL
    if(nz > 0) z <- rep(0, nz)
    if(np > 0) p <- rep(0, np)
    if(nz2 > 0)
    {
      z[1:nz2] <- as.numeric(dat[seq(fz + 2, (fz + 2 * nz2), by = 2)]) + 
                  as.numeric(dat[seq(fz + 3, (fz + 2 * nz2 + 1), by = 2)]) * 1i
    }
    if(np2 > 0)
    {
      p[1:np2] <- as.numeric(dat[seq(fp + 2, (fp + 2 * np2), by = 2)]) + 
                  as.numeric(dat[seq(fp + 3, (fp + 2 * np2 + 1), by = 2)]) * 1i
    }
  }
  for(i in 1:length(s))
  {
    if(!decon)
    {
      # Convolve
      x <- s[[i]]$x; L <- length(x)
      SPS <- 1/s[[i]]$delta
      fn <- SPS/2
      NFFT <- nextn(length(x), 2)
      NumUniquePts <- ceiling((NFFT + 1)/2)
      Z <- array(0, NFFT)
      Z[1:length(x)] <- x; x <- Z
      f <- seq(from = -fn, to = fn, length = NFFT)
      fs <- f[ c((NFFT/2 + 1):NFFT, 1:(NFFT/2))]
      S <- 1i * fs
      H <- pz.tf(p = p, z = z, g = g)
      if(domain == "t")
      {
        tf <- fft(H(S))/NFFT
        fx <- convolve(x, tf)
        s[[i]]$x <- Re(fx)[1:L]
      }else
      {
        FFT <- fft(x)
        fFFT <- H(S) * FFT
        fx <- fft(fFFT, inverse = TRUE)/NFFT
        s[[i]]$x <- Re(fx)[1:L]
      }
    }else
    {
      # Deconvolve
      x <- s[[i]]$x; L <- length(x)
      SPS <- 1/s[[i]]$delta
      fn <- SPS/2
      NFFT <- nextn(length(x), 2)
      NumUniquePts <- ceiling((NFFT + 1)/2)
      Z <- array(0, NFFT)
      Z[1:length(x)] <- x; x <- Z
      f <- seq(from = -fn, to = fn, length = NFFT)
      fs <- f[ c((NFFT/2 + 1):NFFT, 1:(NFFT/2))]
      S <- 1i * fs
      H <- pz.tf(p = p, z = z, g = g)
      FFT <- fft(x)
      fFFT <- FFT / H(S)
      fx <- fft(fFFT, inverse = TRUE)/NFFT
      s[[i]]$x <- Re(fx)[1:L]
    }
  }
  return(s)
}

pz.tf <- function(p = NULL, z = NULL, g = NULL)
{
  if(is.null(g)) g <- 1
  np <- length(p);    nz <- length(z)
  if(np > 0)
  {
    den <- paste("(s - ", p, ")", sep = "", collapse = " * ")
  }else
  {
    den <- 1
  }
  if(nz > 0)
  {
    if(g == 1)
      num <- paste("(s - ", z, ")", sep = "", collapse = " * ")
    else
      num <- paste(g, " * (s - ", z, ")", sep = "", collapse = " * ")
  }else
  {
    num <- g
  }
  funchar <- paste("H <- function(s) ", num, "/(", den, ")", sep = "")
  funexpr <- parse(text = funchar)
  eval(funexpr)
  return(H)
}


tf <- function(type = "BU", n = 1, rp = NULL, rs = NULL, plots = TRUE,
               dB = FALSE)
{
  # Possible types:  BU = Butterworth
  #                  BE = Bessel
  #                  C1 = Chebyshev Type I
  #                  C2 = Chebyshev Type II
  if(!(type == "BU" | type == "BE" | type == "C1" | type == "C2"))
    stop(paste(type, "is not a supported filter type.\n"))
  if(type == "C1")
    if(is.null(rp))
      stop("rp required for type C1")
  if(type == "C2")
    if(is.null(rs))
      stop("rs required for type C2")
  
  tf <- switch(type,
          BU = butpz(n),
          BE = besselpz(n),
          C1 = cheb1pz(n, rp),
          C2 = cheb2pz(n, rs)
              )
  
  if(plots == TRUE){
    if(!dB)
    {
      f <- seq(from = -3, to = 3, length = 500)
      S <- 1i *  f
      H <- pz.tf(p = tf$p, z = tf$z, g = tf$k)
      mag <- abs(H(S))
      phi <- Arg(H(S))
      par(mfrow = c(2, 1))
      plot(f, mag, type = "l", axes = FALSE, ylim = c(0, 1),
           xlab = "", ylab = "Magnitude")
      w <- expression(omega[c])
      axis(side = 1, at = c(-2, -1, 0, 1, 2), lab = c(NA, w, 0, w, NA))
      axis(side = 2, at = c(0, 1))
      plot(f, phi, type = "l", axes = FALSE,
           xlab = "", ylab = "Phase, rad")
      axis(side = 1, at = c(-2, -1, 0, 1, 2), lab = c(NA, w, 0, w, NA))
      axis(side = 2, at = c(-3:3), lab = c(-3, NA, NA, 0, NA, NA, 3))
    }else
    {
      f <- seq(from = 0.1, to = 10, length = 500)
      S <- 1i *  f
      H <- pz.tf(p = tf$p, z = tf$z, g = tf$k)
      mag <- abs(H(S))
      phi <- Arg(H(S))
      par(mfrow = c(2, 1))
      keep <- f > 0
      plot(f, 20 * log10(mag), type = "l", axes = FALSE,
           xlab = "", ylab = "dB", log = "x", xlim = c(0.1, 10),
           ylim = c(min(20 * log10(mag)), 0))
      axis(side = 1)
      axis(side = 2)
      plot(f, phi, type = "l", axes = FALSE,
           xlab = expression(omega / omega[c]),
           ylab = "Phase, rad", log = "x", xlim = c(0.1, 10))
      axis(side = 1)
      axis(side = 2)
    }
  }
  invisible(list(p = tf$p, z = tf$z, k = tf$k))
}
