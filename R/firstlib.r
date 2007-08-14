.First.lib <- function(lib, pkg)
{
  library.dynam("Rsac", pkg, lib)
  cat("Rsac 0.1-4 is loaded.\n")
}
