#' Estimate a release schedule for the series matching a certain string search.
#'
#' This function takes a search string as argument and returns a dataframe containing the estimated date for the next data point.
#' @param get_release_schedule estimate release schedule
#' @keywords estimate release schedule
#' @export
#' @examples
#' get_release_schedule()

get_release_schedule <- function(search_str) {

  datos_path <- gsub("/",
                     "\\\\", tools::R_user_dir("bdeseries", which = "data"))

  series_catalog <- feather::read_feather(paste0(datos_path, "\\catalogo_be.feather"))

  events_df <- calendar::ic_read("https://www.bde.es/webbe/es/estadisticas/compartido/calendario/ics/calendario-bde-2023.ics") |>
    dplyr::rename(fecha_evento = `DTSTART;TZID=Europe/Madrid`,
                  fecha = DTSTAMP,
                  resumen = SUMMARY,
                  descripcion = DESCRIPTION,
                  alt_text = `X-ALT-DESC;FMTTYPE=text/html`,
                  uid = UID) |>
    dplyr::select(-`DTEND;TZID=Europe/Madrid`, -alt_text)


}
