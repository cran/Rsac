spect <- function(x, plot = TRUE, main = NULL, ...)
{
  # See spectrum, plot.spec, spec.pgram, in STATS
  sac2ts <- function(X)
    ts(data = X$amp, start = X$b, frequency = 1/X$dt)
  GetTitle <- function(X)
  {
    sta <- gsub(" ", "", X$sta)
    cmp <- gsub(" ", "", X$comp)
    net <- gsub(" ", "", X$knetwork)
    paste(sta, cmp, net, sep = "-")
  }
  if(!is.null(main) & (length(main) != length(x)) )
    stop("length(x) must equal length(main).\n")
  spect <- vector(mode = "list", length = length(x))
  if(is.null(main))
    main <- sapply(x, GetTitle)
  sac.ts <- lapply(x, sac2ts)
  spect <- lapply(sac.ts, spectrum, plot = FALSE, ...)
  if(plot)
  {
    par(mfrow = c(length(x), 1))
    for(i in 1:length(x))
    {
      spect[[i]] <- spectrum(sac.ts[[i]], plot = TRUE,
                             main = main[i], ...)
    }
  }else
  {
    for(i in 1:length(x))
    {
      spect[[i]] <- spectrum(sac.ts[[i]], plot = FALSE,
                             main = main[i], ...)
    }
  }
  class(spect) <- "rsac-spec"
  invisible(spect)
}
