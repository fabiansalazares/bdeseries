.onLoad <- function(...) {
  packageStartupMessage("bdeseries v0.41-20231120 - miguel@fabiansalazar.es")

  download_series_full()
}
