# GENERAL ----

get_os <-  function(){

  info <- capture.output(sessionInfo())
  os <- info[3]

  if (grepl(pattern = "linux", x = os, ignore.case = T)) {
    return("linux")
  }

  if (grepl(pattern = "windows", x = os, ignore.case = T)) {
    return(("windows"))
  }

  return("Package is Linux and Windows only.")

}

get_sqlite_cli_binary <- function(use_sys_exe = TRUE){

  os <- get_os()

  lib_path <- .libPaths()
  pkgs <- list.dirs(path = lib_path, recursive = F)
  this_pkg <- grep(pattern = "SQLiteRsq", x = pkgs, ignore.case = T, value = T)

  if (os == "linux") {

    if (isTRUE(use_sys_exe) & file.exists("/usr/bin/sqlite3")) return("/usr/bin/sqlite3")

    binary_exe <- "sqlite3"

  }

  if (os == "windows") {

    binary_exe <- "sqlite3.exe"

  }

  slash <- if (length(this_pkg) > 0) {"/"} else {NULL}

  path <- paste0(this_pkg, slash, "bin/", os, "/", binary_exe)

  return(path)

}

# BACKUP ----
db_backup <- function(conn, location = getwd(), ...){

  bkp_path <- paste0(location, "/", basename(conn@db_path), ".bak")

  cat(paste0("Database backed up at: ", bkp_path))

  file.copy(from = conn@db_path, to = bkp_path, ...)

}