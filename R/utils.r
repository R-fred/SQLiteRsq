get_os <-  function(){

  info <- capture.output(sessionInfo())
  os <- info[2]

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

  path <- paste0(this_pkg, "/bin/", os, "/", binary_exe)

  return(path)

}

execute_query <- function(sqlite_conn, option = NULL, db_path, query){

  cmd <- paste0(sqlite_conn@binary, " '", sqlite_conn@db_path, "' ", "'", option, "' ", "'", query, "'")
  output <- system(command = cmd, intern = T)

  return(output)

}

create_query <- function(qry_string, object){

}