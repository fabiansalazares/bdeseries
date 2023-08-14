#' Download the full set of series Banco de España database
#'
#' This function downloads and store the full catalog of Banco de España data series.
#'
#' @keywords download full banco de españa series
#' @export
#' @examples
#' download_series_full()

download_series_full <- function() {
  datos_path <- gsub("/",
                     "\\\\", tools::R_user_dir("bdeseries", which = "data"))

  if (!dir.exists(datos_path)){
    dir.create(datos_path,
               recursive = TRUE)

  }

  catalogo <- dplyr::tibble()

  for (bde_data_set in c("be", "si")) {
    # Downloaded from: https://www.bde.es/webbe/es/estadisticas/recursos/descargas-completas.html
    # be - boletín estadístico
    # si - síntesis de indicadores

    message("Descargando catalogo_", bde_data_set, "...")
    catalogo_set <- utils::read.csv(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/datos/csv/catalogo_", bde_data_set, ".csv"),
                                   encoding="latin1") |>
      dplyr::as_tibble() |>
      dplyr::mutate(nombre = as.character(`Nombre.de.la.serie.`),
                    numero = as.character(`Número.secuencial`),
                    alias = as.character(`Alias.de.la.serie`),
                    fichero = as.character(`Nombre.del.archivo.con.los.valores.de.la.serie`),
                    descripcion = as.character(`Descripción.de.la.serie`),
                    tipo = as.character(`Tipo.de.variable`),
                    unidades = as.character(`Código.de.unidades`),
                    exponente = as.character(`Exponente`),
                    decimales = as.character(`Número.de.decimales`),
                    descripcion_unidades_exponente = as.character(`Descripción.de.unidades.y.exponente`),
                    frecuencia = as.character(`Frecuencia.de.la.serie`),
                    fecha_primera_observacion = as.character(`Fecha.de.la.primera.observación`),
                    fecha_ultima_observacion = as.character(`Fecha.de.la.última.observación`),
                    numero_observaciones = as.character(`Número.de.observaciones`),
                    titulo=as.character(`Título.de.la.serie`),
                    fuente=as.character(`Fuente`),
                    notas=as.character(`Notas`),
                    db = bde_data_set) |>
      dplyr::select(nombre,
                    numero,
                    alias,
                    fichero,
                    descripcion,
                    tipo,
                    unidades,
                    exponente,
                    decimales,
                    descripcion_unidades_exponente,
                    frecuencia,
                    fecha_primera_observacion,
                    fecha_ultima_observacion,
                    numero_observaciones,
                    titulo,
                    fuente,
                    notas,
                    db)

    message(datos_path)

    # feather::write_feather(catalogo_set,
    #                        paste0(datos_path, paste0("\\catalogo_", bde_data_set, ".feather")))

    temp_zipfile <- tempfile()
    message("Descargando ", bde_data_set, ".zip...")

    download.file(paste0("https://www.bde.es/webbe/es/estadisticas/compartido/datos/zip/", bde_data_set, ".zip"),
                  temp_zipfile)

    unzip(temp_zipfile, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = datos_path, unzip = "internal",
          setTimes = FALSE)


    catalogo <- dplyr::bind_rows(catalogo, catalogo_set)
  }

  feather::write_feather(catalogo,
                         paste0(datos_path, paste0("\\catalogo.feather")))


}

