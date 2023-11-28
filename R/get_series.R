#' Get series in the Banco de España database
#'
#' This function returns a tibble containing the series that match the given series' code(s)
#' @param codes series' code(s) to be downloaded
#' @keywords download get series
#' @examples
#' get_series()
#' s@export


get_series <- function(codes,
                       verbose=FALSE,
                       usefulldatabase=FALSE) {

  datos_path <- gsub("/",
                     "\\\\", tools::R_user_dir("bdeseries", which = "data"))


  .series_final_df <- dplyr::tibble()


  for (.code in codes) {

          empty_series_flag <- FALSE

          .series <- dplyr::tibble()

          if(verbose) message("code: ", .code)

          # extracting csv file that contains the series using the general catalog, and generating the full path
          .csv_ficheros_path <-  paste0(datos_path,
                                      "\\",
                                     tolower((bdeseries::catalogo |>
                                                dplyr::filter(nombre == .code) |>
                                                dplyr::distinct(fichero))$fichero))
          # replacing / with \\
          # this should be platform dependent - execute only in windows
          .csv_ficheros_path <- gsub("\\\\",
                                    "/",
                                    .csv_ficheros_path)

          fecha_primera_observacion <- bdeseries::catalogo |>
            dplyr::filter(nombre == .code) |>
            dplyr::distinct(fecha_primera_observacion)

          fecha_ultima_observacion <- bdeseries::catalogo |>
            dplyr::filter(nombre == .code) |>
            dplyr::distinct(fecha_ultima_observacion)

          descripcion <- (bdeseries::catalogo |>
                            dplyr::filter(nombre == .code) |>
                            dplyr::distinct(descripcion))$descripcion

          alias <- (bdeseries::catalogo |>
                      dplyr::filter(nombre == .code) |>
                      dplyr::distinct(alias))$alias

          if (!grepl("Miles de Euros", descripcion) && (descripcion != "Euros") && !is.na(descripcion)) {
            nombre <- descripcion

          } else {
            nombre <- alias
          }


          # some csvs contain "nombre" fields that are empty. This is known to happen -at least- in cuentas financieras files.
          # These empty strings are to be removed if there is an alternative.
          if (length(nombre[!nombre ==""]) > 0) {
            nombre <- nombre[!nombre ==""]
          }

          # some series that appear in different csvs have more than one unique descriptions.
          # to get around this, we concatenate all the descriptions into one single string.
          if (length(nombre) > 1) {
            nombre <- paste(nombre,
                  collapse=" / ")
          }

          # looping over each csv, for each given code
          for(.csv_fichero_path in .csv_ficheros_path) {
                    if(verbose) message(".csv_fichero_path: ", .csv_fichero_path)

                    tryCatch({

                      csv_datos <- read.csv(.csv_fichero_path)
                      # csv_datos <- readr::read_csv(.csv_fichero_path,
                      #                              readr::locale("es", encoding = "utf8"))


                      if (!(.code |> stringr::str_replace("#", ".") |>
                            stringr::str_replace("\\$", ".") |>
                            stringr::str_replace("\\%", ".")) %in% colnames(csv_datos)) {
                        if(verbose) message("La serie ", .code, " no existe en ", .csv_fichero_path)
                        next
                      }

                      .serie <- csv_datos |>
                        tail(nrow(csv_datos) - 6) |>
                        dplyr::rename(fecha = 1) |>
                        dplyr::select(fecha, one_of(stringr::str_replace(.code, "#",".") |>
                                                      stringr::str_replace("\\$", ".") |>
                                                      stringr::str_replace("\\%", "."))) |>
                        dplyr::rename(valores = one_of(stringr::str_replace(.code,"#",".") |>
                                                         stringr::str_replace("\\$", ".") |>
                                                         stringr::str_replace("\\%", "."))) |>
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
                                                             ))) |>
                        dplyr::mutate(nombres = nombre) |>
                        dplyr::mutate(valores = stringr::str_replace(valores, "_", "")) |>
                        dplyr::mutate(valores = as.double(valores)) |>
                        dplyr::as_tibble() |>
                        dplyr::mutate(codigo = .code,
                                      fichero = .csv_fichero_path,
                                      decimales = (bdeseries::catalogo |>
                                                     dplyr::filter(nombre == .code) |>
                                                     dplyr::distinct(decimales))$decimales[[1]],
                                      unidades = (bdeseries::catalogo |>
                                                    dplyr::filter(nombre == .code) |>
                                                    dplyr::distinct(unidades))$unidades[[1]],
                                      exponente = (bdeseries::catalogo |>
                                                     dplyr::filter(nombre == .code) |>
                                                     dplyr::distinct(exponente))$exponente[[1]],
                                      decimales = (bdeseries::catalogo |>
                                                     dplyr::filter(nombre == .code) |>
                                                     dplyr::distinct(decimales))$decimales[[1]],
                                      descripcion_unidades_exponente = (bdeseries::catalogo |>
                                                                          dplyr::filter(nombre == .code) |>
                                                                          dplyr::distinct(descripcion_unidades_exponente))$descripcion_unidades_exponente[[1]],
                                      frecuencia = (bdeseries::catalogo |>
                                                      dplyr::filter(nombre == .code) |>
                                                      dplyr::distinct(frecuencia))$frecuencia[[1]],
                                      fecha_primera_observacion = (bdeseries::catalogo |>
                                                                     dplyr::filter(nombre == .code) |>
                                                                     dplyr::distinct(fecha_primera_observacion))$fecha_primera_observacion[[1]],
                                      fecha_ultima_observacion = (bdeseries::catalogo |>
                                                                    dplyr::filter(nombre == .code) |>
                                                                    dplyr::distinct(fecha_ultima_observacion))$fecha_ultima_observacion[[1]],
                                      numero_observaciones = max((bdeseries::catalogo |>
                                                                dplyr::filter(nombre == .code) |>
                                                                dplyr::distinct(numero_observaciones))$numero_observaciones),
                                      titulo = (bdeseries::catalogo |>
                                                  dplyr::filter(nombre == .code) |>
                                                  dplyr::distinct(titulo))$titulo[[1]],
                                      fuente = (bdeseries::catalogo |>
                                                  dplyr::filter(nombre == .code) |>
                                                  dplyr::distinct(fuente))$fuente,)

                      # message(".serie: ")
                      # print(.serie)

                      # message("serie: nrows(): ", nrow(.serie))
                      },
                      error=function(cond) {
                         if(verbose) message(paste0("Serie ", .code, " could not be retrieved."))
                         if(verbose) message("Error: ", cond)
                         next
                         },
                      # warning=function(cond) {
                      #    message(paste0("Serie ", .code, " returned the following warning message: ", cond))
                      #
                      #   },
                      final={})

                    .serie <- .serie |>
                      dplyr::filter(!is.na(valores))

                    .series <- dplyr::bind_rows(.series, .serie)

                    if(nrow(.serie) == 0) {
                      empty_series_flag <- TRUE
                    } else {
                      empty_series_flag <- FALSE
                    }
          }

          if (nrow(.series) == 0 & empty_series_flag) {
            warning("Serie: ", .code, " is empty.")
            return(NULL)
          }

          # what is the latest date in all the retrieved data?
          max_fecha_series <- max(.series$fecha)

          # after grouping by file of origin (fichero), groups of rows whose latest date is not the latest need to be removed
          .series <- .series |>
            dplyr::group_by(fichero) |>
            dplyr::filter(!max(fecha) < max_fecha_series) |>
            dplyr::ungroup() |>
            dplyr::filter(fichero == dplyr::first(fichero)) |>
            dplyr::distinct()


          # binding the retrieved series to the dataframe to be returned
          .series_final_df <- dplyr::bind_rows(.series_final_df, .series)

  }

  .series_final_df <- .series_final_df |>
    dplyr::arrange(fecha)

  return(.series_final_df)
}
