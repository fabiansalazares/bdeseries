#' Generate a data object containing all the retrievable series in Banco de España series.
#'
#' This function generates a feather and .Rda object containing all the series in Banco de España datasets.
#'
#' @keywords generate data object full database series Banco de España BdE
#' @export
generate_full_database <- function() {

  error_list <- NULL

  .datos_path <- gsub("/",
                     "\\\\",
                     tools::R_user_dir("bdeseries", which = "data"))

  if (!dir.exists(paste0(.datos_path))) { # }, "catalogo.feather"))){
    message("Creating bdeseries data directory...")
    dir.create(.datos_path,
               recursive = TRUE)

  }

  message("Generating full database data object...")

  .code_list <- bdeseries::catalogo |>
    _$nombre

  inicio <- 1

  .pb <- txtProgressBar(min=inicio, max=length(.code_list), style=3)


  # devuelve error: ! Argument 15295 must be a data frame or a named atomic vector.
  .full_series_df <- do.call(dplyr::bind_rows,
                             lapply(inicio:length(.code_list),
                                    function(x) {
                                      setTxtProgressBar(.pb,
                                                        x)

                                      tryCatch({get_series(.code_list[x])},
                                               error = function(e) {error_list <- c(error_list, .code_list[x])})

                                      } ))
  return(.full_series_df)

}
