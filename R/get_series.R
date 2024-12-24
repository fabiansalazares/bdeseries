#' Get series in the Banco de España databas
#' @param codes series' code(s) to be downloaded
#' @keywords download get series
#' @examples get_series("")
#' @returns This function returns a tibble containing the series that match the given series' code(s)
#' @examples get_series(")
#' @export
get_series <- function(codes,
                       verbose=FALSE,
                       usefulldatabase=FALSE) {

  .datos_path <- get_data_path()

  .series_final_df <- lapply(
    X=codes,
    FUN=function(.code) {

      empty_series_flag <- FALSE

      .series <- dplyr::tibble()

      # no hay ninguna serie con este código en el catálogo ----
      if(nrow(bdeseries::catalogo |> dplyr::filter(nombre == .code)) == 0) {
        message("Serie ", .code, " cannot be found. Skipping...")
        return(NULL)
      }

      # extraemos los archivos csv que contienen las series en el catálogo general
      .csv_ficheros_path <-  paste0(
        .datos_path,
        "/",
        tolower((bdeseries::catalogo |>
                   dplyr::filter(nombre == .code) |>
                   dplyr::distinct(fichero))$fichero))

      descripcion <- (bdeseries::catalogo |>
                        dplyr::filter(nombre == .code) |>
                        dplyr::distinct(descripcion))$descripcion

      alias <- (bdeseries::catalogo |>
                  dplyr::filter(nombre == .code) |>
                  dplyr::distinct(alias))$alias


      nombre <- alias
      if (!grepl("Miles de Euros", descripcion) && (descripcion != "Euros") && !is.na(descripcion)) {
        nombre <- descripcion
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

      .series <- lapply(
        X=.csv_ficheros_path,
        FUN=function(.csv_fichero_path) {

          if(verbose) {
            message(".csv_fichero_path: ", .csv_fichero_path)
            message(".code: ", .code)
          }

          .serie <-tryCatch({
            csv_datos <- utils::read.csv(.csv_fichero_path)

            if (!(.code |> stringr::str_replace("#", ".") |>
                  stringr::str_replace("\\$", ".") |>
                  stringr::str_replace("\\%", ".")) %in% colnames(csv_datos)) {
              if(verbose) message("La serie ", .code, " no existe en ", .csv_fichero_path)
              next
            }


            csv_datos |>
              utils::tail(nrow(csv_datos) - 6) |>
              dplyr::rename(fecha = 1) |>
              dplyr::select(fecha, tidyselect::any_of(stringr::str_replace(.code, "#",".") |>
                                            stringr::str_replace("\\$", ".") |>
                                            stringr::str_replace("\\%", "."))) |>
              dplyr::rename(valores = tidyselect::any_of(stringr::str_replace(.code,"#",".") |>
                                               stringr::str_replace("\\$", ".") |>
                                               stringr::str_replace("\\%", "."))) |>
              dplyr::filter(fecha != "FUENTE" & fecha != "NOTAS") |> # & valores != "_") |>
              dplyr::mutate(
                fecha = dplyr::case_when(
                stringr::str_detect(fecha, "ENE") ~ stringr::str_replace(fecha, "ENE", "01"),
                stringr::str_detect(fecha, "FEB") ~ stringr::str_replace(fecha, "FEB", "02"),
                stringr::str_detect(fecha, "MAR") ~ stringr::str_replace(fecha, "MAR", "03"),
                stringr::str_detect(fecha, "ABR") ~ stringr::str_replace(fecha, "ABR", "04"),
                stringr::str_detect(fecha, "MAY") ~ stringr::str_replace(fecha, "MAY", "05"),
                stringr::str_detect(fecha, "JUN") ~ stringr::str_replace(fecha, "JUN", "06"),
                stringr::str_detect(fecha, "JUL") ~ stringr::str_replace(fecha, "JUL", "07"),
                stringr::str_detect(fecha, "AGO") ~ stringr::str_replace(fecha, "AGO", "08"),
                stringr::str_detect(fecha, "SEP") ~ stringr::str_replace(fecha, "SEP", "09"),
                stringr::str_detect(fecha, "OCT") ~ stringr::str_replace(fecha, "OCT", "10"),
                stringr::str_detect(fecha, "NOV") ~ stringr::str_replace(fecha, "NOV", "11"),
                stringr::str_detect(fecha, "DIC") ~ stringr::str_replace(fecha, "DIC", "11"),
                TRUE ~ fecha
              )) |>
              dplyr::mutate(
                fecha = dplyr::if_else(
                  stringr::str_length(fecha) == 4,
                  # format: YYYY
                  sprintf("01 01 %s", fecha),
                  dplyr::if_else(
                    stringr::str_length(fecha) == 7,
                    # format: MM YYYY
                    sprintf(
                      "01 %s %s",
                      stringr::str_sub(fecha, 1,2),
                      stringr::str_sub(fecha, 4,7)
                    ),
                    # format: dd MM YYYY
                    sprintf(
                      "%s %s %s",
                      stringr::str_sub(fecha, 1,2),
                      stringr::str_to_sentence(paste0(stringr::str_sub(fecha, 4,5))),
                      stringr::str_sub(fecha,7,10)
                    )
                  )
                )
              ) |>
              dplyr::mutate(
                fecha = timeDate::timeLastDayInMonth(
                  as.Date(fecha, "%d %m %Y")
                ) |> as.Date()
              ) |>
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
                            fecha_primera_observacion = NA,
                            fecha_ultima_observacion = NA,
                            numero_observaciones = NA,
                            titulo = (bdeseries::catalogo |>
                                        dplyr::filter(nombre == .code) |>
                                        dplyr::distinct(titulo))$titulo[[1]],
                            fuente = (bdeseries::catalogo |>
                                        dplyr::filter(nombre == .code) |>
                                        dplyr::distinct(fuente))$fuente,)

          },
          error=function(cond) {
            if(verbose) message(paste0("Serie ", .code, " could not be retrieved."))
            if(verbose) message("Error: ", cond)
            next
          },
          final={})

          .serie <- .serie |>
            dplyr::filter(!is.na(valores))

          if(nrow(.serie) == 0) {
            return(NULL)
          }

          return(.serie)
        }
      ) |> dplyr::bind_rows()

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

      return(.series)
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::select(fecha, nombres, valores, codigo, unidades, frecuencia, descripcion_unidades_exponente, fuente) |>
    dplyr::arrange(fecha, nombres)

  # if("fecha" %in% names(.series_final_df)) {
  #   .series_final_df <- .series_final_df |>
  #     dplyr::arrange(fecha)
  # }

  return(.series_final_df )
}
