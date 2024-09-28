#' Return the full catalog of Banco de España series.
#' @keywords dump full banco de españa series catalog
#' @examples get_catalog()
#' @export
get_catalog <- function() {

  return(bdeseries::catalogo)

}
