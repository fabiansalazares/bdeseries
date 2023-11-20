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

  ### If directory datos/cf does not exist, create it
  if (!dir.exists(paste0(datos_path, "\\cf"))){
    dir.create(paste0(datos_path, "\\cf"),
               recursive = TRUE)

  }

  # downloading TE_CF.zip
  temp_zipfile_cf <- tempfile()
  message("Descargando TE_CF.zip...")

  download.file(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/datos/zip/TE_CF.zip"),
                temp_zipfile_cf)

  unzip(temp_zipfile_cf, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = paste0(datos_path, "\\cf"), unzip = "internal",
        setTimes = FALSE)


  catalogo <- bind_rows(catalogo,
                       generate_catalog("cf"))

  # to obtain a 1-to-1 relationship between nombres and descripciones, we filter out:
  #     (by order of importance)
  #     1. series whose last observation is not the latest available
  #     1. series whose first observation is not the oldest available
  #     3. series whose numero_observaciones is not the maximum
  #     4. series whose appearance is not the first
  catalogo <- catalogo |>
    dplyr::group_by(nombre) |>
    dplyr::filter(fecha_ultima_observacion == max(fecha_ultima_observacion)) |>
    dplyr::filter(fecha_primera_observacion == min(fecha_primera_observacion)) |>
    dplyr::filter(numero_observaciones == max(numero_observaciones)) |>
    dplyr::arrange(nombre) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::group_by(nombre) |>
    dplyr::filter(fecha_ultima_observacion == max(fecha_ultima_observacion)) |>
    dplyr::filter(fecha_primera_observacion == min(fecha_primera_observacion)) |>
    dplyr::filter(numero_observaciones == max(numero_observaciones)) |>
    dplyr::arrange(descripcion) |>
    dplyr::filter(dplyr::row_number() == 1)

  catalogo <- catalogo |>
    dplyr::group_by(nombre) |>
    dplyr::filter(fecha_ultima_observacion == max(fecha_ultima_observacion)) |>
    dplyr::filter(fecha_primera_observacion == min(fecha_primera_observacion)) |>
    dplyr::filter(numero_observaciones == max(numero_observaciones)) |>
    dplyr::arrange(nombre) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::group_by(descripcion) |>
    dplyr::filter(numero_observaciones == max(numero_observaciones)) |>
    dplyr::filter(fecha_ultima_observacion == max(fecha_ultima_observacion)) |>
    dplyr::filter(fecha_primera_observacion == max(fecha_primera_observacion)) |>
    dplyr::filter(dplyr::row_number() == 1)

  feather::write_feather(catalogo, paste0(datos_path, "\\catalog.feather"))

  usethis::use_data(catalogo, overwrite = TRUE)

  return(catalog)


}
