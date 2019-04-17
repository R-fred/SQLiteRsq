# Class also provides info about tables and fields.
setClass("SQLiteConn",
         slots = list(binary = "character",
                      db_path = "character",
                      conn_string = "character",
                      tables = "list",
                      fields = "list"
                      ),
         prototype = list(binary = .sqlite_bin)
)

setValidity("SQLiteConn", function(object) {
  if (length(object@binary) != 1) {
    "Slot @binary should be a character vector of length 1."
    #return(FALSE)
  } else if (object@binary == "") {
    "Slot @binary cannot contain an empty string."
  } else if (file.exists(object@binary) == FALSE) {
    "SQLite binary not found. Check the path of your sqlite binary."
    #return(FALSE)
  } else if (length(object@db_path) != 1) {
    "Slot @binary should be a character vector of length 1."
    #return(FALSE)
  } else if (object@db_path == "") {
    "Slot @db_path cannot contain an empty string."
  } else if (file.exists(object@db_path) == FALSE) {
    "Database file not found. Check the path of your database file."
    #return(FALSE)
  } else if (grepl(pattern = object@db_path, x = object@conn_string) == FALSE) {
    "Invalid connection string. Check that the value of the slot @db_path is contained in the @conn_string slot."
  } else if (length(object@binary) == 1 & length(object@binary) == 1 & file.exists(object@binary) & file.exists(object@db_path)) {
    return(TRUE)
  }
})

setGeneric("ExecuteStatement", function(ConnObj, qry){standardGeneric("ExecuteStatement")})
setMethod(f = "ExecuteStatement", signature = "SQLiteConn", definition = function(ConnObj, qry){

  isValid <- IsValidSQLiteConnection(ConnObj = ConnObj)

  if (!isTRUE(isValid)) stop("Invalid connection object.\nStopping now.")

  cmd <- sprintf("%s %s", ConnObj@conn_string, qry)

  output <- system(command = cmd, intern = T)

  return(output)

})

setGeneric("IsValidSQLiteConnection", function(ConnObj){standardGeneric("IsValidSQLiteConnection")})
setMethod(f = "IsValidSQLiteConnection", signature = "SQLiteConn", definition = function(ConnObj){
          validObject(ConnObj)
             })

setGeneric("UpdateSQLiteConnection", function(ConnObj, ...){standardGeneric("UpdateSQLiteConnection")})
setMethod(f = "UpdateSQLiteConnection", signature = "SQLiteConn",
         definition = function(ConnObj, ...){
            argts <- list(...)
            if(hasArg(db_path)) ConnObj@db_path <- argts$db_path
            if(hasArg(binary)) ConnObj@binary <- argts$binary
            ConnObj@tables <- chk_tbls(ConnObj)
            ConnObj@fields <- lapply(ConnObj@tables, chk_tbl_headers, conn = ConnObj)

            return(ConnObj)

            })

setGeneric("DeleteSQLiteConnection", function(ConnObj){standardGeneric("DeleteSQLiteConnection")})
setMethod(f = "DeleteSQLiteConnection", signature = "SQLiteConn",
          definition = function(ConnObj){
            en <- parent.env(env = environment())
            eval(parse(text = paste0("rm(\"", deparse(substitute(ConnObj)), "\"", ", envir = ", quote(en), ")")))
            #rm(eval(quote(ConnObj)), envir = parent.frame())
            cat(paste0("Removed connection object: ", deparse(substitute(ConnObj))))
          })

setGeneric("RecoverLastSQLiteConnection", function(ConnObj = .last_sqlite_conn){standardGeneric("RecoverLastSQLiteConnection")})
setMethod(f = "RecoverLastSQLiteConnection", signature = "SQLiteConn",
          definition = function(ConnObj = .last_sqlite_conn){
            return(ConnObj)
          })

SQLConnect <- function(path, binary = .sqlite_bin){

  new_obj <- new(Class = "SQLiteConn", db_path = path, binary = .sqlite_bin)
  new_obj@conn_string <- paste0(new_obj@binary, " ",new_obj@db_path)
  new_obj@tables <- chk_tbls(new_obj)
  new_obj@fields <- lapply(new_obj@tables, chk_tbl_headers, conn = new_obj)

  .last_sqlite_conn <- new_obj

  return(new_obj)
}
