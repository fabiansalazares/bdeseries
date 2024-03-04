.onLoad <- function(...) {
  packageStartupMessage("bdeseries 0.49-20240304 - miguel@fabiansalazar.es")

  download_series_full()
}
