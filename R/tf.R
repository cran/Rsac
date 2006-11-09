tf <- function(type = "BU", n = 1, rp = NULL, rs = NULL, plots = TRUE,
               dB = FALSE){
  # Possible types:  BU = Butterworth
  #                  BE = Bessel
  #                  C1 = Chebyshev Type I
  #                  C2 = Chebyshev Type II
  if(!(type == "BU" | type == "BE" | type == "C1" | type == "C2")){
    stop(paste(type, "is not a supported filter type.\n"))
  }
  if(type == "C1") if(is.null(rp)) stop("rp required for type C1")
  if(type == "C2") if(is.null(rs)) stop("rs required for type C2")
  
  tf <- switch(type,
          BU = buttap(n),
          BE = besselap(n),
          C1 = cheb1ap(n, rp),
          C2 = cheb2ap(n, rs)
              )
  
  if(plots == TRUE){
    if(!dB){
      f <- seq(from = -3, to = 3, length = 500)
      S <- complex(i = 1) *  f
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
    }else{
      f <- seq(from = 0.1, to = 10, length = 500)
      S <- complex(i = 1) *  f
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
