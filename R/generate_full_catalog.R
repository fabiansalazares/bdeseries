#' Generate full catalog from downloaded series.
#'
#' This function generates a catalog from the series contained in all of Banco de España's datasets.
#'
#' @keywords generate catalog series banco de españa
#' @export
#' @examples
#' generate_full_catalog()
#'
generate_full_catalog <- function() {

  message("Generating full catalog...")

  datos_path <- gsub("/",
                     "\\\\",
                     tools::R_user_dir("bdeseries", which = "data"))

  if (!dir.exists(paste0(datos_path))){
    dir.create(datos_path,
               recursive = TRUE)

  }


  catalogo <- tibble()

  catalogo <- generate_catalog(".")

  catalogo <- bind_rows(catalog,
                       generate_catalog("cf"))

  feather::write_feather(catalogo, paste0(datos_path, "\\catalog.feather"))

  usethis::use_data(catalogo, overwrite = TRUE)

  return(catalog)


}
