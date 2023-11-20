#' Dump the full Banco de España series catalog.
#'
#' This function returns the full catalog of Banco de España series.
#'
#' @keywords dump full banco de españa series catalog
#' @export
#' @examples
#' download_seget_catalogries_full()

get_catalog <- function(forcedownload=FALSE) {
  # datos_path <- gsub("/",
  #                    "\\\\",
  #                    tools::R_user_dir("bdeseries", which = "data"))
  #
  # if (!dir.exists(paste0(datos_path))){
  #   dir.create(datos_path,
  #              recursive = TRUE)
  # }
  #
  # message("Date of last update: ",as.Date(file.info(paste0(datos_path, "\\catalogo.feather"))$mtime) )

  # return(feather::read_feather(paste0(datos_path, "\\catalogo.feather")))
  return(catalogo)

}
