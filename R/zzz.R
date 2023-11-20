.onLoad <- function(...) {
  packageStartupMessage("bdeseries v0.44-20231120 - miguel@fabiansalazar.es")

  download_series_full()
}
