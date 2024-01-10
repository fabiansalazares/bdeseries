.onLoad <- function(...) {
  packageStartupMessage("bdeseries v0.47-20240110 - miguel@fabiansalazar.es")

  download_series_full()
}
