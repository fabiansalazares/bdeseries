#' Update the series.
#'
#' This function downloads the datasets from Banco de España and replaces the existing CSV files containing the series (if any).
#'
#' @keywords update download full banco de españa series
#' @export
#' @examples
#' update_series()
#'
#'
#'
update_series <- function() {

  datos_path <- gsub("/",
                     "\\\\",
                     tools::R_user_dir("bdeseries", which = "data"))


  if (!dir.exists(paste0(datos_path))) { # }, "catalogo.feather"))){
    message("Creating bdeseries data directory...")
    dir.create(datos_path,
               recursive = TRUE)

  }

  for (bde_data_set in c("be", # boletín estadístico
                         "si", # síntesis de indicadores
                         # "cf", # cuentas financieras - hay errores en el catálogo: al menos en una serie (DMZ10S80ZH0_TPRPIB.Q), el fichero csv al que apunta el catalogo de series no existe.
                         #       # comprobado que hay al menos 20 archivos csv referenciados en el catálogo que empiezan por "cf*" pero que luego no existen.
                         "ti", # tipos de interés - el zip con los datos de tipos de interés no tiene el csv correspondiente a ti_1_2.csv
                         "pb")) { # encuesta de préstamos bancarios
    # Downloaded from: https://www.bde.es/webbe/es/estadisticas/recursos/descargas-completas.html

    temp_zipfile <- tempfile()
    message("Descargando ", bde_data_set, ".zip...")

    download.file(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/datos/zip/", bde_data_set, ".zip"),
                  temp_zipfile)

    unzip(temp_zipfile, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = datos_path, unzip = "internal",
          setTimes = FALSE)

    # Three options regarding catalog:
    #     - Use Banco de España catalogs. -> forceusebdecatalog = TRUE, forcegeneratecatalog = FALSE
    #     - (BY DEFAULT) Use pre-generated catalog from the series -> forceusebdecatalog = FALSE, forcegeneratecatalog = FALSE
    #     - Generate a new catalog from the series. -> forceusebdecatalog = FALSE, forcegeneratecatalog = TRUE
    # if(forceusebdecatalog & forcegeneratecatalog) {
    #   message("Both forceusebdecatalog and forceforcegeneratecatalog are set to TRUE. Aborting.")
    # }

    # if (forceusebdecatalog) {
    #   message("Descargando catalogo_", bde_data_set, "...")
    #   catalogo_set <- utils::read.csv(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/datos/csv/catalogo_", bde_data_set, ".csv"),
    #                                  encoding="latin1") |>
    #     dplyr::as_tibble() |>
    #     dplyr::mutate(nombre = as.character(`Nombre.de.la.serie.`),
    #                   numero = as.character(`Número.secuencial`),
    #                   alias = as.character(`Alias.de.la.serie`),
    #                   fichero = as.character(`Nombre.del.archivo.con.los.valores.de.la.serie`),
    #                   descripcion = as.character(`Descripción.de.la.serie`),
    #                   tipo = as.character(`Tipo.de.variable`),
    #                   unidades = as.character(`Código.de.unidades`),
    #                   exponente = as.character(`Exponente`),
    #                   decimales = as.character(`Número.de.decimales`),
    #                   descripcion_unidades_exponente = as.character(`Descripción.de.unidades.y.exponente`),
    #                   frecuencia = as.character(`Frecuencia.de.la.serie`),
    #                   fecha_primera_observacion = as.character(`Fecha.de.la.primera.observación`),
    #                   fecha_ultima_observacion = as.character(`Fecha.de.la.última.observación`),
    #                   numero_observaciones = as.character(`Número.de.observaciones`),
    #                   titulo=as.character(`Título.de.la.serie`),
    #                   fuente=as.character(`Fuente`),
    #                   notas=as.character(`Notas`),
    #                   db = bde_data_set) |>
    #     dplyr::select(nombre,
    #                   numero,
    #                   alias,
    #                   fichero,
    #                   descripcion,
    #                   tipo,
    #                   unidades,
    #                   exponente,
    #                   decimales,
    #                   descripcion_unidades_exponente,
    #                   frecuencia,
    #                   fecha_primera_observacion,
    #                   fecha_ultima_observacion,
    #                   numero_observaciones,
    #                   titulo,
    #                   fuente,
    #                   notas,
    #                   db) |>
    #     dplyr::mutate(fecha_ultima_observacion = lubridate::dmy(dplyr::case_when(fecha_ultima_observacion %in% c("...", "") | is.na(fecha_ultima_observacion) ~ NA,
    #                                                               stringr::str_length(fecha_ultima_observacion) == 4 ~ paste0("01 enero ", fecha_ultima_observacion),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "ENE" ~  paste0("01 enero ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "FEB" ~  paste0("01 febrero ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "MAR" ~  paste0("01 marzo ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "ABR" ~  paste0("01 abril ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "MAY" ~  paste0("01 mayo ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "JUN" ~  paste0("01 junio ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "JUL" ~  paste0("01 julio ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "AGO" ~  paste0("01 agosto ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "SEP" ~  paste0("01 septiembre ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "OCT" ~  paste0("01 octubre ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "NOV" ~  paste0("01 noviembre ", stringr::str_sub(fecha_ultima_observacion, 5,8)),
    #                                                        stringr::str_sub(fecha_ultima_observacion,1,3) == "DIC" ~  paste0("01 diciembre ", stringr::str_sub(fecha_ultima_observacion, 5,8)))),
    #                   fecha_primera_observacion = lubridate::dmy(dplyr::case_when(fecha_primera_observacion %in% c("...", "") | is.na(fecha_primera_observacion) ~ NA,
    #                                                                              stringr::str_length(fecha_primera_observacion) == 4 ~ paste0("01 enero ", fecha_primera_observacion),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "ENE" ~  paste0("01 enero ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "FEB" ~  paste0("01 febrero ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "MAR" ~  paste0("01 marzo ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "ABR" ~  paste0("01 abril ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "MAY" ~  paste0("01 mayo ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "JUN" ~  paste0("01 junio ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "JUL" ~  paste0("01 julio ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "AGO" ~  paste0("01 agosto ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "SEP" ~  paste0("01 septiembre ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "OCT" ~  paste0("01 octubre ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "NOV" ~  paste0("01 noviembre ", stringr::str_sub(fecha_primera_observacion, 5,8)),
    #                                                                              stringr::str_sub(fecha_primera_observacion,1,3) == "DIC" ~  paste0("01 diciembre ", stringr::str_sub(fecha_primera_observacion, 5,8)))))
    #
    #   message(datos_path)
    #
    #   catalogo <- dplyr::bind_rows(catalogo, catalogo_set)
    #
    # }
  }

}
