#' Estimate a release schedule for the series matching a certain string search.
#'
#' This function takes a search string as argument and returns a dataframe containing the estimated date for the next data point.
#' @param get_release_schedule estimate release schedule
#' @keywords estimate release schedule
#' @export
#' @examples
#' get_release_schedule()

get_release_schedule <- function(search_str) {

  # Descargar de: https://www.bde.es/webbe/es/estadisticas/compartido/calendario/ics/calendario-bde.ics

  events_df <- calendar::ic_read(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/calendario/ics/calendario-bde.ics")) |>
    dplyr::rename(fecha_evento = `DTSTART;TZID=Europe/Madrid`,
                  fecha = DTSTAMP,
                  resumen = SUMMARY,
                  descripcion = DESCRIPTION,
                  alt_text = `X-ALT-DESC;FMTTYPE=text/html`,
                  uid = UID) |>
    dplyr::select(-`DTEND;TZID=Europe/Madrid`, -alt_text) |>
    dplyr::mutate(fecha_evento = stringr::str_sub(fecha_evento, 1,8) |> as.Date(format="%Y%m%d"),
                  fecha = stringr::str_sub(fecha, 1,8) |> as.Date(format="%Y%m%d"))
    # dplyr::mutate(fecha_evento = lubridate::ymd(fecha_evento),
    #               fecha = lubridate::ymd(fecha))

  return(events_df)

}
