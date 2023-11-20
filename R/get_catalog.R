#' Dump the full Banco de España series catalog.
#'
#' This function returns the full catalog of Banco de España series.
#'
#' @keywords dump full banco de españa series catalog
#' @export
#' @examples
#' get_catalog()

get_catalog <- function(forcedownload=FALSE) {

  return(bdeseries::catalogo)

}
