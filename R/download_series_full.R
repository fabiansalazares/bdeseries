#' Download the full set of series Banco de España database
#'
#' This function downloads and stores the full catalog of Banco de España data series.
#'
#' @keywords download full banco de españa series
#' @export
#' @examples download_series_full()

download_series_full <- function(forcedownload=FALSE,
                                 forcegeneratecatalog=FALSE,
                                 forcecfdownload=FALSE,
                                 forcegeneratecatalogcf=FALSE,
                                 forceusebdecatalog=FALSE) {

  .datos_path <- get_data_path()

  if(length(list.files(.datos_path, pattern="csv")) == 0) {
    update_series()
    return()
  }

  if (!is.na(as.Date((file.info(paste0(.datos_path, "\\", list.files(.datos_path, pattern="csv") |> sample(1))))$mtime))) {
    message("Date of last update: ", as.Date((file.info(paste0(.datos_path, "\\", list.files(.datos_path, pattern="csv") |> sample(1))))$mtime) )
    if (as.Date((file.info(paste0(.datos_path, "\\", list.files(.datos_path, pattern="csv") |> sample(1))))$mtime) == Sys.Date() & !forcedownload) {
      message("BdE data have already been downloaded today.")
      return()
    } else {
      update_series()
    }
  }
}

