.onLoad <- function(...) {
  packageStartupMessage("bdeseries 0.50-20240927 - miguel@fabiansalazar.es")

  if(!getOption("bdeseries_disable_autoupdate")) {
    download_series_full()
  } else {
    message("Auto update has been disabled. To update the series' database run: download_series_full()")
  }

}
