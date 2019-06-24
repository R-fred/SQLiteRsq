# Class also provides info about tables and fields.
#
# Methods to create the objects ----
setClass("SQLiteConn",
         slots = list(binary = "character",
                      db_path = "character",
                      conn_string = "character",
                      conn_strg_no_opts = "character",
                      options = "list",
                      pragma = "list",
                      tables = "list",
                      fields = "list",
                      sha3sum = "character" #TODO(): Needs to be updated after each change in the database.
                      )
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

setMethod("initialize", signature  = "SQLiteConn",
           definition = function(.Object,
                                  db_path,
                                  binary = .sqlite_bin,
                                  options = list(headers = "on",
                                                 mode = "list"),
                                 ...
                                 ) {

             opt_strg <- create_options_string(options)

             .Object@binary <- binary
             .Object@db_path <- db_path
             .Object@conn_string <- paste0(.Object@binary, " ", .Object@db_path, " ", opt_strg)
             .Object@conn_strg_no_opts <- paste0(.Object@binary, " ", .Object@db_path)
             .Object@tables <- chk_tbls(.Object)
             .Object@fields <- lapply(.Object@tables, chk_tbl_headers, conn = .Object)
             .Object@sha3sum <- system(paste0(.Object@conn_string, " '.sha3sum'"), intern = T)[2]
             .Object@options <- options

             .Object <- callNextMethod()
             validObject(.Object)
             return(.Object)
           })

setMethod("show", signature = "SQLiteConn",
          definition = function(object){
            msg <- paste0("\n--- SQLite connection ---------------------------\n",
                          "\nSHA3 sum: ", object@sha3sum,
                          "\nBinary: ", object@binary,
                          "\nDatabase path: ", object@db_path,
                          "\nOptions: ", create_options_string(object@options),
                          "\nPragmas: ", object@pragma,
                          "\nConnection string: ", object@conn_string, "\n",
                          "\n--- Specifics ------------------------------------\n",
                          "\nNumber of tables: ", length(object@tables),
                          "\nTables (with column names): \n"
            )
            cat(msg)
            print(x = object@fields)

          })

NewSQLiteConnection <- function(path, binary = .sqlite_bin, ...){

  new_obj <- new(Class = "SQLiteConn", db_path = path, binary = binary, ...)

  .last_sqlite_conn <<- new_obj

  cat("\nCreated following database connection:\n")
  show(new_obj)

  return(new_obj)
}

# Accessors -----
setGeneric("binary", function(ConnObj){standardGeneric("binary")})
setMethod(f = "binary", signature = "SQLiteConn", definition = function(ConnObj){
  ConnObj@binary
})

setGeneric("db_path", function(ConnObj){standardGeneric("db_path")})
setMethod(f = "db_path", signature = "SQLiteConn", definition = function(ConnObj){
  ConnObj@db_path
})

setGeneric("conn_string", function(ConnObj){standardGeneric("db_path")})
setMethod(f = "conn_string", signature = "SQLiteConn", definition = function(ConnObj){
  ConnObj@conn_string
})

setGeneric("options", function(ConnObj){standardGeneric("options")})
setMethod(f = "options", signature = "SQLiteConn", definition = function(ConnObj){
  ConnObj@options
})

setGeneric("pragma", function(ConnObj){standardGeneric("pragma")})
setMethod(f = "pragma", signature = "SQLiteConn", definition = function(ConnObj){
  ConnObj@pragma
})

setGeneric("tables", function(ConnObj){standardGeneric("tables")})
setMethod(f = "tables", signature = "SQLiteConn", definition = function(ConnObj){
  ConnObj@tables
})

setGeneric("fields", function(ConnObj){standardGeneric("fields")})
setMethod(f = "fields", signature = "SQLiteConn", definition = function(ConnObj){
  ConnObj@fields
})

setGeneric("sha3sum", function(ConnObj){standardGeneric("sha3sum")})
setMethod(f = "sha3sum", signature = "SQLiteConn", definition = function(ConnObj){
  ConnObj@sha3sum
})


# Setters ----
setGeneric("binary<-", function(ConnObj, value){standardGeneric("binary<-")})
setMethod(f = "binary<-", signature = "SQLiteConn", definition = function(ConnObj, value){

  ConnObj@binary <- value
  validObject(ConnObj)

  ConnObj

})

setGeneric("db_path<-", function(ConnObj, value){standardGeneric("db_path<-")})
setMethod(f = "db_path<-", signature = "SQLiteConn", definition = function(ConnObj, value){

  ConnObj@db_path <- value
  validObject(ConnObj)

  ConnObj

})

setGeneric("sha3sum<-", function(ConnObj){standardGeneric("sha3sum<-")})
setMethod(f = "sha3sum<-", signature = "SQLiteConn", definition = function(ConnObj){

  ConnObj@sha3sum <- system(paste0(ConnObj@conn_string, " '.sha3sum'"), intern = T)[2]

  validObject(ConnObj)

  return(ConnObj)

})

# Methods to work with objects ----

setGeneric("GetQueryResults", function(ConnObj, qry, dataTable = F, default = T){standardGeneric("GetQueryResults")})
setMethod(f = "GetQueryResults", signature = "SQLiteConn", definition = function(ConnObj, qry, dataTable = F, default = T){

  data.table::fread(cmd = ExecuteStatement(ConnObj = ConnObj, qry = qry, default = default), sep = "|")


})

setGeneric("InsertData", function(ConnObj, data){standardGeneric("InsertData")})
setMethod(f = "InsertData", signature = "SQLiteConn", definition = function(ConnObj, data){

  # Run Update Connection method @ end to update sha3sum.
  # Get data and check if it is a vector, data frame, list, etc...
  # Check the number of columns if a data frame.
  # check that the names of the data columns are the same and in the same order as in the DB table.
  # Check that the number of values to insert are equal to the number of columns in the dataset.
  # If not, either:
  # - No value for a column: add NULL. NA -> NULL
  # - No value add name to the INSERT STATEMENT.
  #   Query becomes: INSERT INTO table (col1, col2, ...) VALUES (val1, val2, ...)
  #
  # Data frames rows should be converted to character vector of length 1.
  # Use transactions: BEGIN TRANSACTION; COMMIT;
  # Example: sqlite3 tests/testthat/test_create_db.sqlite 'BEGIN TRANSACTION;' 'INSERT INTO tbl VALUES("r4c1", "r4c2", "r4c3");' 'INSERT INTO tbl VALUES("r5c1", "r5c2", "r5c3");' 'INSERT INTO tbl VALUES("r6c1", NULL, "r6c3");' 'COMMIT;'



})

setGeneric("ExecuteStatement", function(ConnObj, qry, default = F){standardGeneric("ExecuteStatement")})
setMethod(f = "ExecuteStatement", signature = "SQLiteConn", definition = function(ConnObj, qry, default = F){

  # THOUGHTS:
  # COUNT(*) statement can be done with .headers off. Otherwise COUNT(*) is returned as header.
  # .headers on should be used at all times except for COUNT(*) in order to allow for JOINS.
  # Use 'AS n_row' to get a proper header for COUNT(*).

  isValid <- IsValidSQLiteConnection(ConnObj = ConnObj)

  if (!isTRUE(isValid)) stop("Invalid connection object.\nStopping now.")

  if (!grepl("'.*'", qry, perl = T)) qry <- paste0("'", qry, "'")

  cmd <- sprintf("%s %s", ConnObj@conn_string, qry) #TODO(): change '.headers on' with option string.

  if (!isTRUE(default)){
    system(command = cmd, intern = T)
  } else {
    return(cmd)
  }

  # if (grepl("COUNT(.*)", qry)) output <- as.numeric(output)

})

setGeneric("IsValidSQLiteConnection", function(ConnObj){standardGeneric("IsValidSQLiteConnection")})
setMethod(f = "IsValidSQLiteConnection", signature = "SQLiteConn", definition = function(ConnObj){
          validObject(ConnObj)
             })

setGeneric("UpdateSQLiteConnection", function(ConnObj, ...){standardGeneric("UpdateSQLiteConnection")})
setMethod(f = "UpdateSQLiteConnection", signature = "SQLiteConn", definition = function(ConnObj, ...){
            argts <- list(...)
            # Modify the slots according to the arguments passed.
            if(hasArg(db_path)) ConnObj@db_path <- argts$db_path
            if(hasArg(binary)) ConnObj@binary <- argts$binary
            if(hasArg(options)) ConnObj@options <- argts$options
            if(hasArg(db_path) & hasArg(binary)) ConnObj@conn_string <- paste0(ConnObj@binary, " ", ConnObj@db_path, " ", create_options_string(ConnObj@options))
            if(hasArg(db_path) & hasArg(binary) & hasArg(options)) ConnObj@conn_strg_no_opts <- paste0(ConnObj@binary, " ", ConnObj@db_path)

            # recreate remaining slots.

            #ConnObj@tables <- chk_tbls(ConnObj) # not working -> find a way. NECESSARY?
            #ConnObj@fields <- lapply(ConnObj@tables, chk_tbl_headers, conn = ConnObj) -> not working, find a way.
            #Split the update in several steps?
            ConnObj@sha3sum <- system(paste0(.Object@conn_string, " '.sha3sum'"), intern = T)[2]

            validObject(ConnObj)

            return(ConnObj)

            })

setGeneric("DeleteSQLiteConnection", function(ConnObj){standardGeneric("DeleteSQLiteConnection")})
setMethod(f = "DeleteSQLiteConnection", signature = "SQLiteConn", definition = function(ConnObj){
            en <- parent.env(env = environment())
            eval(parse(text = paste0("rm(\"", deparse(substitute(ConnObj)), "\"", ", envir = ", quote(en), ")")))
            #rm(eval(quote(ConnObj)), envir = parent.frame())
            cat(paste0("Removed connection object: ", deparse(substitute(ConnObj))))
          })

RecoverLastSQLiteConnection <- function(ConnObj = .last_sqlite_conn){
            return(ConnObj)
          }

setGeneric("InsertCSV", function(ConnObj, table_name){standardGeneric("InsertCSV")})
setMethod(f = "InsertCSV", signature = "SQLiteConn", definition = function(ConnObj, table_name){
  # Need to check for correct table names.
  # Have options to check only number of columns?
  # Have option to choose the columns to import?
  # Call UpdateConnection method in order to update sha3sum.
  #
  #sqlite3 'tests/testthat/test_create_db.sqlite' '.mode csv' '.import trial.csv tbl'
})

# Old versions ----

setGeneric("GetQueryResults_old", function(ConnObj, qry, dataTable = F){standardGeneric("GetQueryResults_old")})
setMethod(f = "GetQueryResults_old", signature = "SQLiteConn", definition = function(ConnObj, qry, dataTable = F){

  res <- ExecuteStatement(ConnObj = ConnObj, qry = qry)

  if (length(res == 0)) output <- NULL
  if (length(res >= 1)) {

    output <- lapply(res, strsplit, split = "\\|") # Results are returned separated by '|'
    names_output <- as.character(unlist(output[[1]])) # headers are in the first row.
    output[[1]] <- NULL


    output <- unlist(output, recursive = F) # List is two levels with headers on.

    output <- lapply(output, function(x) as.data.frame(t(x), stringsAsFactors = F)) #t(as.list(x))

    if (isTRUE(dataTable)) {
      output <- data.table::rbindlist(output)
      output <- data.table::as.data.table(output)
    } else {
      output <- do.call("rbind", output)
    }

    names(output) <- names_output


  }

  output <- rapply(output, convert_to_numeric, how = "replace")

  return(output)

})
