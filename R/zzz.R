.onLoad <- function(...) {
  packageStartupMessage("bdeseries v0.46-20231127 - miguel@fabiansalazar.es")

  download_series_full()
}
