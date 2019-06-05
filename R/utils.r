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

exists2 <- function(object){

  if (!is.character(object)) stop("\nObject should be of type character.\n")

  object %in% ls(all.names = T, envir = parent.frame())
}

clean_col_headers <- function(col_names){
  # Function to clean up column names.
  # Generates db friendly column names.
  # e.g. changes '.', '-', ' ' into '_'; removes ':', ';', ',', '|', '>', '<', '=', etc...
}

# CONVERT DATA FRAMES TO CHARACTER VECTORS ----

convert_dt_to_input_string <- function(data){

  data <- data.table::as.data.table(data) #ensure that the data is a data.table.

  data <- setDT(data)[, sql := Reduce(function(...) paste(..., sep = ", "), .SD), .SDcols = names(data)] # create sql string for each row.

  output <- convert_char_to_input_string(data[, sql])

  # INSERT ' and complete VALUES(...) part of the insert string.
  # adding the table to insert into needs to be handled later since the job here is only to convert to character vector.
  return(output)

}

convert_char_to_input_string <- function(data){

  if (sum(grepl(pattern = ", ", x = data)) == 0) {
    output <- paste(data, sep = "", collapse = ", ")
  } else {
    output <- data
  }

  output <- paste0("VALUES('", gsub(x = output, pattern = ", ", replacement = "', '"), "')")

  output <- gsub(pattern = "'NA'", replacement = "NULL", x = output)

  return(output)

}

# BACKUP ----

setGeneric("BackUpDB", function(ConnObj, ...){standardGeneric("BackUpDB")})
setMethod(f = "BackUpDB", signature = "SQLiteConn", definition = function(ConnObj, location = getwd(), ...){

  bkp_path <- paste0(location, "/", basename(ConnObj@db_path), ".bak")
  success <- file.copy(from = ConnObj@db_path, to = bkp_path, ...)

  if (isTRUE(success)) {
    cat(paste0("Database backed up at: ", bkp_path, ".\n"))
    return(TRUE)
  } else {
    cat(paste0("Database could not be backed up at: ", bkp_path, ".\n", "Check that the database path is valid.\n"))
    return(FALSE)
  }

})