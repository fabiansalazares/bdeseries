.onLoad <- function(...) {
  packageStartupMessage("bdeseries v0.48-20240111 - miguel@fabiansalazar.es")

  download_series_full()
}
