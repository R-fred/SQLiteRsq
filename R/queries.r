# Main functions ----

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

# Tables ----

chk_tbl_exists <- function(conn, tbl){

# OBSOLTE NOW THAT THE TABLES ARE +

  qry <- sprintf("SELECT name FROM sqlite_master WHERE type='table' AND name='%s';", tbl)




}

#' chk_tbl_headers
#'
#' @name chk_tbl_headers
#'
chk_tbl_headers <- function(conn, tbl){

  cmd <- paste0(conn@conn_string, " ","\"PRAGMA table_info(", tbl,")\"")

  res <- system(command = cmd, intern = TRUE)
  res <- strsplit(x = res, split = "\\|")
  res <- rapply(res[2:length(res)], function(x) x[2])

  return(res)

}

#' chk_tbls
#'
#' @name chk_tbls
#'
chk_tbls <- function(conn){

  cmd <- paste0(conn@conn_string, " ", ".tables")

  res <- system(command = cmd, intern = TRUE)
  res <- strsplit(x = res, split = "\\s+")
  res <- rapply(res, function(x) x)

  names_res <- res

  res <- as.list(res)
  names(res) <- names_res

  return(res)

}

# Delete ----

qry_delete <- function(tbl, ...){

}

# Insert ----

qry_insert <- function(tbl, type = c("values", "select", "default"), values){

  qry_vals <- values

  qry <- "\"INSERT INTO tbl VALUES('ID1', 'value1', 'value2')\";"

  # Several inserts in a row can be done by separating the INSERT statements with a space.
  # Count the number of records before and an after insertion. Can be done by using "SELECT COUNT(ROWID) FROM tbl;"

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

# Select ----

# Overarching function - break down in manageable pieces.
# Only works for a single table at present.
# How to select all related info in several tables? Via foreign keys? How to get to them?
retrieve_data_from_db <- function(conn, table, operator = c("=", ">", "<", "LIKE"), columns = "*", distinct = FALSE, limit = NULL, order_by = NULL, db_disconnect = FALSE, order_number = NULL, sample_name = NULL, analytical_lab_number = NULL, date = NULL, requestor_lastname = NULL, requestor_firstname = NULL, ...){

  # Get arguments
  argg <- c(as.list(environment()), list(...))

  # Extract filters and their values
  filters <- argg[names(argg) != "conn" & names(argg) != "table" & names(argg) != "columns" & names(argg) != "distinct" & names(argg) != "limit" & names(argg) != "operator" & names(argg) != "db_disconnect" & names(argg) != "order_by"]
  filters[sapply(filters, is.null)] <- NULL
  filters[which(tolower(operator) == "like")] <- lapply(filters[which(tolower(operator) == "like")], function(x) paste0("%", x, "%"))

  fields <- names(filters)

  filters <- lapply(filters, function(x) paste0("'", x, "'"))
  names(filters) <- fields

  # Warning if length of operator not the same as number of arguments
  if (length(operator) != length(filters) & length(filters) > 0) warning("\nYou have specified an insufficient number of operators.\nThe first operator will be recycled.\nIf this is not desired, please specify an operator for each filter.\n")

  # Create query string - For dates, no possibility right now to do BETWEEN.
  # No possibility to do >, < for a same filter parameter.
  filter_qry <- vector(mode = "list", length = length(filters))
  for (ii in seq_along(filters)) {
    filter_qry[[ii]] <- paste(
      names(filters)[ii],
      ifelse(length(operator)> 1, operator[ii], operator[1]),
      filters[[ii]],
      collapse = " OR ",
      sep = " "
    )
    filter_qry[[ii]] <- paste0("(", filter_qry[[ii]], ")")
  }

  filter_qry <- paste(filter_qry, collapse = " AND ", sep = " ")
  filter_qry <- paste0(" WHERE ", filter_qry)

  if (length(fields) == 0) filter_qry <- NULL

  # Limit statement
  if (!is.null(argg$limit) & is.numeric(argg$limit)) {
    limit_qry <- paste0(" LIMIT ", argg$limit)
  } else {
    limit_qry <- NULL
  }

  # Distinct statement
  if (isTRUE(argg$distinct)) {
    distinct_qry <- " DISTINCT "
  } else {
    distinct_qry <- NULL
  }

  # Order by statement
  if (!is.null(argg$order_by)) {
    order_by_cols <- paste(argg$order_by, collapse = ", ", sep = "")
    order_by_qry <- paste0(" ORDER BY ", order_by_cols)
  } else {
    order_by_qry <- NULL
  }

  # Columns
  columns_qry <- paste(argg$columns, collapse = ", ", sep = " ")

  # Build the statement and sanitize the query
  qry <- DBI::sqlInterpolate(conn = conn, sql = paste0("SELECT ",
                                                       distinct_qry,
                                                       columns_qry,
                                                       " FROM ",
                                                       table,
                                                       filter_qry,
                                                       order_by_qry,
                                                       limit_qry,
                                                       ";")
  )
  # Run the query
  res <- RSQLite::dbGetQuery(conn = conn, statement = qry)

  if (isTRUE(argg$db_disconnect)) RSQLite::dbDisconnect(conn = conn)

  return(res)

}

qry_select <- function(tbl, col){

}

qry_select_file <- function(selection_qry){

  qry <- "select writefile('C:/Users/fches/DSC5527.ARW', value_2) FROM tbl WHERE ID='ID3';"

}

# Update ----

qry_update <- function(tbl, ...){

}

qry_upsert <- function(tbl, ...){

}

# Where ----
qry_where <- function(field, value, comparison = c(">", "<", "=", "LIKE")){

  #check for type of value.
  #Only accept, character or numeric
  #value inserted depending on type.

  qry <- paste0("WHERE ", field, "=", "'",value, "'")
  qry <- paste0("WHERE ", field, "=", value)

}
