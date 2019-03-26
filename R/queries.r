# Main functions ----
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

# Support functions ----

chk_tbl_exists <- function(conn, tbl){

  qry <- sprintf("")

}

chk_tbl_headers <- function(conn, tbl){

  qry <- "SELECT * FROM tbl LIMIT 0;"

}

qry_delete <- function(tbl, ...){

}

qry_insert <- function(tbl, type = c("values", "select", "default"), values){

  qry_vals <- values

  qry <- "\"INSERT INTO tbl VALUES('ID1', 'value1', 'value2')\";"

}

qry_insert_values <- function(){

}

qry_insert_select <- function(){

}

qry_insert_default <- function(){

}

qry_insert_file <- function(file_name){

  qry <- "insert into tbl VALUES('ID3', 'value4', readfile('C:/Users/fches/Pictures/Wilfried/_DSC5527.ARW'));"

}

qry_select <- function(tbl, col){

}

qry_select_file <- function(selection_qry){

  qry <- "select writefile('C:/Users/fches/DSC5527.ARW', value_2) FROM tbl WHERE ID='ID3';"

}

qry_update <- function(tbl, ...){

}

qry_upsert <- function(tbl, ...){

}

qry_where <- function(field, value, comparison = c(">", "<", "=", "LIKE")){

  #check for type of value.
  #Only accept, character or numeric
  #value inserted depending on type.

  qry <- paste0("WHERE ", field, "=", "'",value, "'")
  qry <- paste0("WHERE ", field, "=", value)

}