.onLoad <- function(...) {
  packageStartupMessage("bdeseries 0.60-20240928 - miguel@fabiansalazar.es")

  .datos_path <- gsub("/",
                      "\\\\",
                      tools::R_user_dir("bdeseries", which = "data"))
  # creamos directorios si no existen
  # esto debería hacerse en la carga del paquete una sola vez
  if (!dir.exists(paste0(.datos_path))){
    dir.create(.datos_path,
               recursive = TRUE)
  }

  if (!dir.exists(paste0(.datos_path, "\\cf"))){
    dir.create(paste0(.datos_path, "\\cf"),
               recursive = TRUE)
  }

  option_bdeseries_disable_autoupdate <- getOption("bdeseries_disable_autoupdate")

  if(is.null(option_bdeseries_disable_autoupdate)) {
      download_series_full()
  } else if(!option_bdeseries_disable_autoupdate) {
      download_series_full()
  } else {
    message("Auto update is disabled by. To manually update the series' database run: download_series_full()")
  }

}
