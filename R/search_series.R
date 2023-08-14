#' Search for series in the Banco de España database
#'
#' This function takes a list of search strings and returns a list of dataframes containing the results for each search string.
#'
#' By default, search_series() matches field "descripcion" of the Banco de España series catalog. However, the field to be matched
#' can be modified by passing values [ | ] to argument 'field'. I.e.: search_series(search_str=c("economía internacional", "España"), field=)
#' @param search_str search string(s) to be matched.
#' @keywords search series
#' @export
#' @examples
#' search_series()

search_series <- function(search_str,
                          field="descripcion") {

  datos_path <- gsub("/",
                     "\\\\", tools::R_user_dir("bdeseries", which = "data"))

  # series_catalog <- feather::read_feather(paste0(datos_path, "\\catalogo_be.feather"))
  series_catalog <- feather::read_feather(paste0(datos_path, "\\catalogo.feather"))

  lapply(search_str, function(x) {
    series_catalog |> dplyr::filter(grepl(stringr::str_replace(x, " ", "_"),
                                                                        descripcion,
                                                                        ignore.case=TRUE))
    }
         )

}
