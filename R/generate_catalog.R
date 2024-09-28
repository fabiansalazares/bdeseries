#' Generate a catalog from a folder
#'
#' This function generates a series catalog from a list of csv files containing Banco de España series.
#'
#' @keywords download full banco de españa series
#' @export
#' @examples
#' generate_catalog()


generate_catalog <- function(directory,
                             db="") {

  message("Generating catalog from ", directory)

  .datos_path <- gsub("/",
                     "\\\\",
                     tools::R_user_dir("bdeseries", which = "data"))

  catalogo <- dplyr::tibble()

  csv_files <- fs::dir_ls(paste0(.datos_path, "\\", directory), glob="*.csv")
  csv_file_counter <- 0
  csv_file_total <- length(csv_files)
  offset_serie <- 0

  catalogo <- lapply(
    X=csv_files,
    function(.x) {
      cat(
        sprintf("\rFicheros procesados: %d \t %d \t - \t %s",
                csv_file_counter,
                csv_file_total,
                scales::number_format(accuracy=0.01, decimal.mark=",", big.mark=".", suffix="%", scale=1e2)(csv_file_counter/csv_file_total)
        )
      )

      utils::flush.console()


      if (stringr::str_detect(.x, "catalogo")) {
        message("Skipping catalogo*.csv")
        return()
      }


      csv_datos <- readr::read_csv(
        .x,
        # csv_datos <- readr::read_csv(.x,
        locale = readr::locale("es",
                               encoding = "latin1"),
        trim_ws=TRUE,
        name_repair="minimal",
        show_col_types = FALSE
      )

      # remove duplicated column names from the csv file
      csv_datos <- csv_datos[ , !duplicated(colnames(csv_datos))]

      csv_datos_procesado <- csv_datos |>
        tail(nrow(csv_datos) - 6) |>
        dplyr::rename(fecha=1) |>
        dplyr::filter(fecha != "FUENTE" & fecha != "NOTAS") |> # & valores != "_") |>
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
    short_csv_format <- FALSE

    # some csvs do not contain FUENTE
    withoutfuente <- FALSE

    if (csv_datos[4,1] != "DESCRIPCIÓN DE LAS UNIDADES") {
      # some csvs contain only three headers, and then continue to having dates:
      if (stringr::str_detect(csv_datos_procesado[4], "FUENTE") |  stringr::str_detect(csv_datos_procesado[5], "FUENTE")) {
        offset_serie <- 1
      # } else if(csv_datos_procesado[[1]][5] == "FRECUENCIA" | csv_datos_procesado[[1]][4]) { # HAY QUE SER CAPACES DE DETECTAR ENE 1962 Y SIMILARES CON UNA EXPRESIÓN REGULAR, EN FILA 4
        # cuando la tercera fila contiene una fecha
        } else if(stringr::str_detect(csv_datos_procesado[[1]][4], "\\b\\d{4}\\b")) {
        short_csv_format <- TRUE
      }
    } else {
      offset_serie <- 0
    }

    series_en_csv_df <- lapply(
      X=(names(csv_datos)) |> _[-1],
      FUN=function(columna) {

      descripcion <- stringr::str_remove(csv_datos[[columna]][3], pattern="Descripción de la DSD:")
      alias <- as.character(csv_datos[[columna]][2])

      # some series' descriptions contain a description of the unit instead of the description itself
      # in these cases, alias is used to fill up descripcion field.
      if (!grepl("Miles de Euros", descripcion) &
          (descripcion != "Euros") &
          (descripcion != "Años") &
          (descripcion != "Monedas") &
          (descripcion != "Billetes") &
          (descripcion != "Porcentaje") &
          (!is.na(descripcion))) {
        descripcion <- descripcion
      } else {
        descripcion <- alias
      }

      serie_cf <- dplyr::tibble(nombre=columna,
                                numero=as.character(csv_datos[[columna]][1]),
                                alias=alias,
                                fichero=.x,
                                descripcion=descripcion,
                                tipo="",
                                unidades=dplyr::if_else(short_csv_format,
                                                 "",
                                                 dplyr::if_else(is.na(csv_datos[[columna]][4+offset_serie]),
                                                                "",
                                                                as.character(csv_datos[[columna]][4+offset_serie]))),
                                exponente="",
                                decimales="",
                                descripcion_unidades_exponente="",
                                frecuencia=dplyr::if_else(short_csv_format,
                                                   "",
                                                   dplyr::if_else(is.na(csv_datos[[columna]][5+offset_serie]),
                                                                  "",
                                                                  as.character(csv_datos[[columna]][5+offset_serie]))),
                                fecha_primera_observacion=fecha_minima,
                                fecha_ultima_observacion=fecha_maxima,
                                numero_observaciones=nrow(csv_datos_procesado[columna]),
                                titulo="",
                                fuente=as.character(csv_datos[[columna]][length(csv_datos[[columna]])-1]),
                                notas="",
                                db=db)
      return(serie_cf)

      }
    ) |> dplyr::bind_rows()

    csv_file_counter <<- csv_file_counter + 1

    return(series_en_csv_df)
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::ungroup() |>
    dplyr::mutate( # extraer de la ruta al fichero todos los directorios
      fichero = stringr::str_extract(fichero, "(?<=bdeseries/).*$")
    )

  browser()

  # remove duplicated column names again from the full catalog
  catalogo <- catalogo[ , !duplicated(colnames(catalogo))]

  return(catalogo)

}
