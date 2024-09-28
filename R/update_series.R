#' This function downloads the datasets from Banco de España and replaces the existing CSV files containing the series (if any).
#' @keywords update download full banco de españa series
#' @examples update_series()
#' @export
update_series <- function() {

  .datos_path <- get_data_path()

  list_of_zip_files <- c(
    "be", # boletín estadístico
    "si", # síntesis de indicadores
    # "cf", # cuentas financieras - hay errores en el catálogo: al menos en una serie (DMZ10S80ZH0_TPRPIB.Q), el fichero csv al que apunta el catalogo de series no existe.
    #       # comprobado que hay al menos 20 archivos csv referenciados en el catálogo que empiezan por "cf*" pero que luego no existen.
    "ti", # tipos de interés - el zip con los datos de tipos de interés no tiene el csv correspondiente a ti_1_2.csv
    "pb",
    "TE_CF"
    )

  lapply(
    X=list_of_zip_files,
    FUN=function(.bde_data_set) {
      temp_zipfile <- tempfile()
      message("Descargando ", .bde_data_set, ".zip...")

      utils::download.file(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/datos/zip/", .bde_data_set, ".zip"),
                    temp_zipfile)

      exdir_path <- ifelse(
        .bde_data_set == "TE_CF",
        paste0(.datos_path, "/cf"),
        .datos_path
      )

      utils::unzip(
        temp_zipfile,
        overwrite = TRUE,
        list=TRUE,
        junkpaths = FALSE,
        exdir = exdir_path,
        unzip = "internal",
        setTimes = FALSE
        )

      utils::unzip(
        temp_zipfile,
        overwrite = TRUE,
        # list=TRUE,
        junkpaths = FALSE,
        exdir = exdir_path,
        unzip = "internal",
        setTimes = FALSE
        )

      message(.bde_data_set, ": número de archivos ", fs::dir_ls(exdir_path) |> length())
    }
  ) |> invisible()


}
