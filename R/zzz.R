.onLoad <- function(...) {
  packageStartupMessage("bdeseries v0.45-20231122 - miguel@fabiansalazar.es")

  download_series_full()
}
