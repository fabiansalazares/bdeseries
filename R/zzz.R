.onLoad <- function(...) {
  packageStartupMessage("bdeseries 0.62-20241223 - miguel@fabiansalazar.es")

  .datos_path <- get_data_path()

  # creamos directorios si no existen
  # esto debería hacerse en la carga del paquete una sola vez

  if (!dir.exists(paste0(.datos_path))){
    message(sprintf("%s not found", .datos_path))
    message("Creating...")
    dir.create(.datos_path,
               recursive = TRUE)
  }

  if (!dir.exists(paste0(.datos_path, "/cf"))){
    message(sprintf("%s not found", paste0(.datos_path, "/cf")))
    message("Creating...")

    dir.create(paste0(.datos_path, "/cf"),
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

get_data_path <- function() {
  bdeseries_data_env_var <- Sys.getenv("BDESERIES_DATA_PATH")

  if(bdeseries_data_env_var != ""){
    return(bdeseries_data_env_var)
  }

  gsub(
    "\\\\",
    "/",
    tools::R_user_dir("bdeseries", which = "data"))
}
