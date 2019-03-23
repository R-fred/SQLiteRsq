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

execute_query <- function(sqlite_conn, option = NULL, db_path, query, append){

  cmd <- paste0(sqlite_conn@binary, " '", sqlite_conn@db_path, "' ", "'", option, "' ", "'", query, "'")
  output <- system(command = cmd, intern = T)

  return(output)

  # append: add only new records to the database.
  # Either implement in SQL code, or check before in R.
  # Dynamic choice? i.e. from a certain number of rows, what is the fastest option?


}

create_query <- function(qry_type, table, object, values){

  # qry_type: defines if INSERT, DELETE, UPDATE, UPSERT, SELECT
  # based on that, call the proper function to put query together.
  #
  # table: table to execute the query against
  #
  # object: optional, the object to use, can be a dataframe
  #
  # values: single values to enter in database
  #

}

chk_tbl_exists <- function(conn, tbl){

}

chk_tbl_headers <- function(conn, tbl){

}

qry_delete <- function(tbl, ...){

}

qry_insert <- function(tbl,){

}

qry_select <- function(tbl, col){

}

qry_update <- function(tbl, ...){

}

qry_upsert <- function(tbl, ...){

}