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

# setGeneric("SQLiteConnect", function(file_path, ...){standardGeneric("SQLiteConnect")})
# setMethod(f = "SQLiteConnect",
#           signature = "SQLiteConnection",
#           definition = function(file_path, ...){
#             obj <- SQLConnect(path = file_path, ...)
#             }
#           )

SQLConnect <- function(path, binary = .sqlite_bin){

  new_obj <- new(Class = "SQLiteConn", db_path = path, binary = .sqlite_bin)
  new_obj@conn_string <- paste0(new_obj@binary, " ",new_obj@db_path)
  new_obj@tables <- chk_tbls(new_obj)
  new_obj@fields <- lapply(new_obj@tables, chk_tbl_headers, conn = new_obj)
  return(new_obj)
}
