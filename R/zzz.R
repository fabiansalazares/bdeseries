.onLoad <- function(...) {
  packageStartupMessage("bdeseries v0.1 - miguel@fabiansalazar.es")

  download_series_full()
}
