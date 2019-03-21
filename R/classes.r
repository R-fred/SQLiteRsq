setClass("SQLiteConn",
         representation(binary = "character", db_path = "character"),
         prototype = list(binary = .sqlite_bin)
)

setGeneric("CreateConnectString", function(x){standardGeneric("CreateConnectString")})
setMethod("CreateConnectString", c(x = "SQLiteConn"),function(x){return(paste0(x@binary, " ",x@db_path))})

SQLConnect <- function(path){

  new_obj <- new(Class = "SQLiteConn", db_path = path)

  return(new_obj)
}