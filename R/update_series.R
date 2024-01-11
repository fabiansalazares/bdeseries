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

  .datos_path <- gsub("/",
                     "\\\\",
                     tools::R_user_dir("bdeseries", which = "data"))


  if (!dir.exists(paste0(.datos_path))) { # }, "catalogo.feather"))){
    message("Creating bdeseries data directory...")
    dir.create(.datos_path,
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
          junkpaths = FALSE, exdir = .datos_path, unzip = "internal",
          setTimes = FALSE)


  }


  ## Cuentas financieras
  if (!dir.exists(paste0(.datos_path, "\\cf"))) { # }, "catalogo.feather"))){
    message("Creating bdeseries data directory...")
    dir.create(.datos_path,
               recursive = TRUE)

  }

  # Downloaded from: https://www.bde.es/webbe/es/estadisticas/recursos/descargas-completas.html

  temp_cf_zipfile <- tempfile()

  message("Descargando TE_CF.zip...")

  download.file(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/datos/zip/TE_CF.zip"),
                temp_cf_zipfile)

  unzip(temp_cf_zipfile, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = paste0(.datos_path, "\\cf"), unzip = "internal",
        setTimes = FALSE)

}
