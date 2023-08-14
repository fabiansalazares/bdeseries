#' Get series in the Banco de España database
#'
#' This function returns a tibble containing the series that match the given series' code(s)
#' @param codes series' code(s) to be downloaded
#' @keywords download get series
#' @examples
#' get_series()
#' s@export


get_series <- function(codes) {

  datos_path <- gsub("/",
                     "\\\\", tools::R_user_dir("bdeseries", which = "data"))

  # if(!file.exists(paste0(datos_path, "\\catalogo_be.feather"))) {
  if(!file.exists(paste0(datos_path, "\\catalogo.feather"))) {

    message("The series and the series' catalog were not found.")
    message("Downloading...")

    download_series_full()
  }


  # series_catalog <- feather::read_feather(paste0(datos_path, "\\catalogo_be.feather"))
  series_catalog <- feather::read_feather(paste0(datos_path, "\\catalogo.feather"))


  csv_fichero_path <-  paste0(datos_path,
                              "\\",
                             tolower((series_catalog |>
                                        dplyr::filter(nombre == codes) |>
                                        dplyr::distinct(fichero))$fichero))

  csv_datos <- read.csv(csv_fichero_path)

  nombre <- (series_catalog |>
                     dplyr::filter(nombre == codes) |>
                     dplyr::distinct(descripcion))$descripcion

  serie <- csv_datos |>
    tail(nrow(csv_datos) - 6) |>
    dplyr::rename(fecha = NOMBRE.DE.LA.SERIE) |>
    dplyr::select(fecha, one_of(codes)) |>
    dplyr::rename(valores = one_of(codes)) |>
    dplyr::filter(fecha != "FUENTE" & fecha != "NOTAS" & valores != "_") |>
    # mutate(fecha_raw = fecha) %>%
    #tail(100) %>%
    dplyr::mutate(fecha = dplyr::if_else(stringr::str_length(fecha) == 4,
                           as.Date(paste0("01 01 ", fecha), format="%d %m %Y"),
                           as.Date(timeDate::timeLastDayInMonth(as.Date(paste0("01 ",
                                                                               stringr::str_to_sentence(paste0(stringr::str_sub(fecha, 1,3),
                                                                                                      ". ",
                                                                                                      stringr::str_sub(fecha,5,8)))),
                                                                        "%d %b %Y"))))) |>
    dplyr::mutate(nombres = nombre) |>
    dplyr::mutate(valores = as.double(valores)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(codigo = codes,
                  fichero = (series_catalog |>
                               dplyr::filter(nombre == codes) |>
                               dplyr::distinct(decimales))$decimales,
                  decimales = (series_catalog |>
                                        dplyr::filter(nombre == codes) |>
                                        dplyr::distinct(decimales))$decimales,
                  unidades = (series_catalog |>
                                dplyr::filter(nombre == codes) |>
                                dplyr::distinct(unidades))$unidades,
                  exponente = (series_catalog |>
                                dplyr::filter(nombre == codes) |>
                                dplyr::distinct(exponente))$exponente,
                  decimales = (series_catalog |>
                                 dplyr::filter(nombre == codes) |>
                                 dplyr::distinct(decimales))$decimales,
                  descripcion_unidades_exponente = (series_catalog |>
                                 dplyr::filter(nombre == codes) |>
                                 dplyr::distinct(descripcion_unidades_exponente))$descripcion_unidades_exponente,
                  frecuencia = (series_catalog |>
                                 dplyr::filter(nombre == codes) |>
                                 dplyr::distinct(frecuencia))$frecuencia,
                  fecha_primera_observacion = (series_catalog |>
                                 dplyr::filter(nombre == codes) |>
                                 dplyr::distinct(fecha_primera_observacion))$fecha_primera_observacion,
                  fecha_ultima_observacion = (series_catalog |>
                                 dplyr::filter(nombre == codes) |>
                                 dplyr::distinct(fecha_ultima_observacion))$fecha_ultima_observacion,
                  numero_observaciones = (series_catalog |>
                                 dplyr::filter(nombre == codes) |>
                                 dplyr::distinct(numero_observaciones))$numero_observaciones,
                  titulo = (series_catalog |>
                                 dplyr::filter(nombre == codes) |>
                                 dplyr::distinct(titulo))$titulo,
                  fuente = (series_catalog |>
                              dplyr::filter(nombre == codes) |>
                              dplyr::distinct(fuente))$fuente,
                  )

    return(serie)
}
