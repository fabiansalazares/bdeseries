.onLoad <- function(...) {
  packageStartupMessage("bdeseries 0.51-20240928 - miguel@fabiansalazar.es")

  if(is.null(getOption("bdeseries_disable_autoupdate")) | !getOption("bdeseries_disable_autoupdate")) {
      download_series_full()
  } else {
    message("Auto update is disabled by. To manually update the series' database run: download_series_full()")
  }

}
