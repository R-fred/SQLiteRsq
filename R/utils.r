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

  data[, sql := paste(unlist(.SD), collapse = ", ")] # create sql string for each row.

  output <- paste(data[, sql], sep = "", collapse = ", ")

  # INSERT ' and complete VALUES(...) part of the insert string.
  # adding the table needs to be handled later since the job here is only to convert to character vector.
  # paste0("VALUES('", stringr::str_replace_all(results_test$sql, pattern = ", ", replacement = "', '"), "')")


  return(output)

}

# TODO(): Create a supporting function to convert data.frames to character vectors
# How to make an efficient version of this function (with data.table?):
# for (ii in 1:nrow(results_test)) {
#     print(as.character(unlist(results_test[ii])))
# }
#
# Here is the solution:
#   - with data.table: results_test[, sql := paste(unlist(.SD), collapse = ", ")]
#   - with base R:
#         cols <- c("b", "c", "d") # column names.
#         data$x <- do.call(paste, c(data[cols], sep=","))
#         for (co in cols) data[co] <- NULL # if want to get rid of columns.
#
#

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