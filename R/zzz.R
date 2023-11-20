.onLoad <- function(...) {
  packageStartupMessage("bdeseries v0.42-20231120 - miguel@fabiansalazar.es")

  download_series_full()
}
