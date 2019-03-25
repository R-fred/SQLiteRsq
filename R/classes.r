# Class also provides info about tables and fields.
setClass("SQLiteConn",
         representation(binary = "character", db_path = "character", tables, fields),
         prototype = list(binary = .sqlite_bin)
)

setGeneric("CreateConnectString", function(x){standardGeneric("CreateConnectString")})
setMethod("CreateConnectString", c(x = "SQLiteConn"),function(x){return(paste0(x@binary, " ",x@db_path))})

SQLConnect <- function(path){

  new_obj <- new(Class = "SQLiteConn", db_path = path)

  return(new_obj)
}