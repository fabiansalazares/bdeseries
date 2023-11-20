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
  # message("Saving into ", outputfile)

  datos_path <- gsub("/",
                     "\\\\",
                     tools::R_user_dir("bdeseries", which = "data"))

  if (!dir.exists(paste0(datos_path, "catalogo.feather"))){
    dir.create(datos_path,
               recursive = TRUE)
  }

  catalogo <- dplyr::tibble()

  # ### If directory datos/cf does not exist, create it
  if (!dir.exists(paste0(datos_path, "\\cf"))){
    dir.create(paste0(datos_path, "\\cf"),
               recursive = TRUE)

  }


  # Processing csvs i TE_CF.zip to extract the series contained and generate catalog.
  for(csv_cf_datos_path in list.files(paste0(datos_path, "\\", directory), pattern=".csv")) {
    message("CSV file: ", csv_cf_datos_path)

    if (stringr::str_detect(csv_cf_datos_path, "catalogo")) {
      message("Skipping catalogo*.csv")
      next
    }

    csv_datos <- readr::read_csv(paste0(datos_path, "\\", directory, "\\", csv_cf_datos_path),
                                 # csv_datos <- readr::read_csv(csv_cf_datos_path,
                                 locale = readr::locale("es",
                                                        encoding = "latin1"),
                                 trim_ws=TRUE,
                                 name_repair="minimal")
    # remove duplicated column names from the csv file
    csv_datos <- csv_datos[ , !duplicated(colnames(csv_datos))]


    csv_datos_procesado <- csv_datos |>
      tail(nrow(csv_datos) - 6) |>
      # dplyr::rename(fecha = `NOMBRE DE LA SERIE`) |>
      dplyr::rename(fecha=1) |>
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


    for (columna in (names(csv_datos)) |> _[-1]) {
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


      message("serie: ", columna)

      serie_cf <- dplyr::tibble(nombre=columna,
                                numero=as.character(csv_datos[[columna]][1]),
                                # alias=as.character(csv_datos[[columna]][2]),
                                alias=alias,
                                # fichero=paste0(datos_path, "\\", directory, "\\", csv_cf_datos_path),
                                fichero=paste0(directory, "\\", csv_cf_datos_path),
                                # descripcion=stringr::str_remove(csv_datos[[columna]][3], pattern="Descripción de la DSD:"),
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
                                # fuente=dplyr::if_else(short_csv_format | withoutfuente,
                                #                "",
                                #                csv_datos[[columna]][length(csv_datos[[columna]])-1]),
                                notas="",
                                db=db)

      catalogo <- dplyr::bind_rows(catalogo, serie_cf)
    }
  }

  # remove duplicated column names again from the full catalog
  catalogo <- catalogo[ , !duplicated(colnames(catalogo))]

  # remove duplicated descripcion s that point to series with less tha




  return(catalogo)

}
