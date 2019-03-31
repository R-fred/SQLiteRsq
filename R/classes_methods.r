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

setGeneric("IsValidSQLiteConnection", function(ConnObj){standardGeneric("IsValidSQLiteConnection")})
setMethod(f = "IsValidSQLiteConnection",
         signature = "SQLiteConn",
         definition = function(ConnObj){
          validObject(ConnObj)
             }
         )

setGeneric("UpdateSQLiteConnection", function(ConnObj, ...){standardGeneric("UpdateSQLiteConnection")})
setMethod(f = "UpdateSQLiteConnection",
         signature = "SQLiteConn",
         definition = function(ConnObj, ...){
            argts <- list(...)
            if(hasArg(db_path)) ConnObj@db_path <- argts$db_path
            if(hasArg(binary)) ConnObj@binary <- argts$binary
            ConnObj@tables <- chk_tbls(ConnObj)
            ConnObj@fields <- lapply(ConnObj@tables, chk_tbl_headers, conn = ConnObj)

            return(ConnObj)

            }
         )

SQLConnect <- function(path, binary = .sqlite_bin){

  new_obj <- new(Class = "SQLiteConn", db_path = path, binary = .sqlite_bin)
  new_obj@conn_string <- paste0(new_obj@binary, " ",new_obj@db_path)
  new_obj@tables <- chk_tbls(new_obj)
  new_obj@fields <- lapply(new_obj@tables, chk_tbl_headers, conn = new_obj)
  return(new_obj)
}
