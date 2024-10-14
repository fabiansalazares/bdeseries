#' Download cuenta financiera series from Banco de España.
#'
#' This function downloads, stores and generates a catalog of the series contained in "Cuentas Financieras" dataset
#'
#' @keywords download full banco de españa series
#' @export
#' @examples
#' download_series_cuenta_financiera()

download_series_cuenta_financiera <- function(forcedownload=FALSE,
                                              forcegeneratecatalogcf=FALSE) {

  message("Downloading cuentas financieras...")

  .datos_path <- gsub("/",
                     "\\\\",
                     tools::R_user_dir("bdeseries", which = "data"))

  if (!dir.exists(paste0(.datos_path))){
    dir.create(.datos_path,
               recursive = TRUE)

  }

  message("Date of last update: ",as.Date(file.info(paste0(.datos_path, "\\catalogo_cf.feather"))$mtime) )

  if (!is.na(as.Date(file.info(paste0(.datos_path, "\\catalogo_cf.feather"))$mtime))) {
    if (as.Date(file.info(paste0(.datos_path, "\\catalogo_cf.feather"))$mtime) >= (Sys.Date() - lubridate::days(31)) & !forcedownload) {
      message("BdE Cuentas Financieras data have already been downloaded today.")
      return(feather::read_feather(paste0(.datos_path, "\\catalogo_cf.feather")))
    } else {

      if (forcedownload) { message("BdE Cuentas Financieras is available, but will be downloaded anyways because forcedownload=TRUE.") }
      if (as.Date(file.info(paste0(.datos_path, "\\catalogo_cf.feather"))$mtime) <= (Sys.Date() - lubridate::days(31))) {
        message("Cuentas financieras series' catalog is older than 31 days. ")
        forcegeneratecatalogcf <- TRUE
        # ifelse(tolower(readline("Would you like to generate catalog for cuentas financieras (it will take a few minutes) ? (Y/n) ") %in% c("y", "yes", "s", "sí", "si"),
        #                ),
        #        forcegeneratecatalogcf <- TRUE,
        #        forcegeneratecatalogcf <- FALSE
        #        )

      }
    }
  } else {
    message("BdE cuentas financieras catalog is not available. Downloading and generating...")
  }

  ## Cuentas Financieras - building a special catalogue for series available in TE_CF.zip (https://www.bde.es/webbe/es/estadisticas/recursos/descargas-completas.html and https://www.bde.es/webbe/es/estadisticas/compartido/datos/zip/TE_CF.zip)
  catalogo_cf <- dplyr::tibble()

  ### If directory datos/cf does not exist, create it
  if (!dir.exists(paste0(.datos_path, "\\cf"))){
    dir.create(paste0(.datos_path, "\\cf"),
               recursive = TRUE)

  }

  # downloading TE_CF.zip
  temp_zipfile_cf <- tempfile()
  message("Descargando TE_CF.zip...")

  download.file(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/datos/zip/TE_CF.zip"),
                temp_zipfile_cf)

  unzip(temp_zipfile_cf, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = paste0(.datos_path, "\\cf"), unzip = "internal",
        setTimes = FALSE)

  if (forcegeneratecatalogcf) {
    # Processing csvs i TE_CF.zip to extract the series contained and generate catalog.
    for(csv_cf_.datos_path in list.files(paste0(.datos_path, "\\cf"))) {
      message("CSV file: ", csv_cf_.datos_path)

      csv_datos <- readr::read_csv(paste0(.datos_path, "\\cf\\", csv_cf_.datos_path),
      # csv_datos <- readr::read_csv(csv_cf_.datos_path,
                                locale = readr::locale("es",
                                                encoding = "latin1"),
                                trim_ws=TRUE,
                                name_repair="minimal")
      # remove duplicated column names from the csv file
      csv_datos <- csv_datos[ , !duplicated(colnames(csv_datos))]

      # csv_datos <- read.csv(paste0(.datos_path, "\\cf\\",  csv_cf_.datos_path))
      # csv_datos_rw <- read.csv(paste0(.datos_path, "\\cf\\",  csv_cf_.datos_path))

      csv_datos_procesado <- csv_datos |>
        tail(nrow(csv_datos) - 6) |>
        dplyr::rename(fecha = `NOMBRE DE LA SERIE`) |>
        # dplyr::select(fecha, one_of(stringr::str_replace(code   ,"#",".") |> stringr::str_replace("\\$", ".") )) |>
        # dplyr::rename(valores = one_of(stringr::str_replace(code,"#",".") |> stringr::str_replace("\\$", "."))) |>
        dplyr::filter(fecha != "FUENTE" & fecha != "NOTAS") |> # & valores != "_") |>
        # mutate(fecha_raw = fecha) %>%
        #tail(100) %>%
        dplyr::mutate(fecha = dplyr::if_else(stringr::str_length(fecha) == 4,
                                             as.Date(paste0("01 01 ", fecha), format="%d %m %Y"),
                                             dplyr::if_else(stringr::str_length(fecha) == 8,
                                                            as.Date(timeDate::timeLastDayInMonth(as.Date(paste0("01 ",
                                                                                                                stringr::str_to_sentence(paste0(stringr::str_sub(fecha, 1,3),
                                                                                                                                                ". ",
                                                                                                                                                stringr::str_sub(fecha,5,8)))),
                                                                                                         "%d %b %Y"))),
                                                            as.Date(paste0(stringr::str_sub(fecha, 1,2),
                                                                           " ",
                                                                           stringr::str_to_sentence(paste0(stringr::str_sub(fecha, 4,6))),
                                                                           ". ",
                                                                           stringr::str_sub(fecha,8,11)), format="%d %b %Y")
                                             )))

      fecha_minima <- min(csv_datos_procesado$fecha)
      fecha_maxima <- max(csv_datos_procesado$fecha)

      # some csvs have a malformed structure in which row 4 of first column does not contain the units, but the description
      # while having at row 3 a (possibly) irrelevant description.
      # this needs to be accounted for. If it's the case, variable offset_serie will be set to one
      if (csv_datos[4,1] != "DESCRIPCIÓN DE LAS UNIDADES") {
        offset_serie <- 1
      } else {
        offset_serie <- 0
      }

      for (columna in (names(csv_datos)) |> _[-1]) {
        message("serie: ", columna)

        serie_cf <- dplyr::tibble(nombre=columna,
                           numero=csv_datos[[columna]][1],
                           alias=csv_datos[[columna]][2],
                           fichero=paste0("cf/", csv_cf_.datos_path),
                           descripcion=stringr::str_remove(csv_datos[[columna]][3], pattern="Descripción de la DSD:"),
                           tipo="",
                           unidades=csv_datos[[columna]][4+offset_serie],
                           exponente="",
                           decimales="",
                           descripcion_unidades_exponente="",
                           frecuencia=csv_datos[[columna]][5+offset_serie],
                           fecha_primera_observacion=fecha_minima,
                           fecha_ultima_observacion=fecha_maxima,
                           numero_observaciones=nrow(csv_datos_procesado[columna]),
                           titulo="",
                           fuente=csv_datos[[columna]][length(csv_datos[[columna]])-1],
                           notas="",
                           db="cf")

        catalogo_cf <- dplyr::bind_rows(catalogo_cf, serie_cf)
      }
    }

    # remove duplicated column names again from the full catalog
    catalogo_cf <- catalogo_cf[ , !duplicated(colnames(catalogo_cf))]

    # save catalog_cf to feather
    feather::write_feather(catalogo_cf, paste0(.datos_path, paste("\\catalogo_cf.feather")))

  } else {
    return(feather::read_feather(paste0(.datos_path, "\\catalogo_cf.feather")))
  }

  return(catalogo_cf)


}

