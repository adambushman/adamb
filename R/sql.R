#' Establish MySQL connection
#'
#' `mysql_connect()` creates a connection with a MySQL database
#' @param username A string representing the user's name
#' @param password A string representing the user's password
#' @param dbname A string representing the database name to which to connect
#' @param host A string representing the server host for the database; defaults to
#' common DigitalOcean host
#' @param port An integer representing the server port for the database; defaults to
#' common DigitalOcean port
#' @param cname A string representing the connection variable name for the environment;
#' defaults to "con"
#' @returns A connection object for use with additional MySQL functions from the package
#' @examples
#' mysql_connect("keychain_user", "keychain_pass", "defaultdb")
#'
#' # Pass custom connection details
#' mysql_connect("keychain_user", "keychain_pass", "defaultdb", "myhost", "myport", "mycname")
#' @import DBI RMariaDB
#' @export
mysql_connect <- function(username = NULL, password = NULL, dbname = NULL, host = NULL, port = NULL, cname = NULL) {
  if(is.null(username) | is.null(password) | is.null(dbname)) {
    stop("Enter valid credentials for `username`, `password`, and `dbname`.")
  }

  connection = ifelse(is.null(cname), "con", cname)

  tryCatch(
    {
      con <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        user = username,
        password = password,
        host = ifelse(!is.null(host), host, "adambushman-db-mysql-do-user-14046221-0.b.db.ondigitalocean.com"),
        port = ifelse(!is.null(port), port, "25060"),
        dbname = dbname
      )

      assign(connection, con, envir = .GlobalEnv)
    },
    error = function(e) {
      print(paste0("ERROR: ", e))
    },
    warning = function(w) {
      print(paste0("WARNING: ", w))
    }
  )
}

#' Peek MySQL database tables
#'
#' `mysql_peek_tables()` lists the table names present from a valid MySQL database connection
#' @seealso [mysql_connect()]
#' @param cname A string representing the connection variable name from the environment;
#' @returns A vector of database table names
#' @examples
#' mysql_peek_tables("mycname")
#'
#' # If connection was made with default cname from `mysql_connect()`, it may be called
#' # with no parameters
#' mysql_peek_tables()
#' @import DBI
#' @export
mysql_peek_tables <- function(cname = NULL) {
  if(is.null(cname) & !exists("con")) {
    stop("Enter a valid connection name or setup a default connection via `mysql_connect()`")
  }

  if(!is.null(cname) & !exists(cname)) {
    stop(paste0("Cannot find the connection `", cname, "` in the environment."))
  }

  connection = ifelse(is.null(cname), "con", cname)

  tryCatch(
    {
      DBI::dbListTables(get(connection))
    },
    error = function(e) {
      print(paste("ERROR:", e))
    },
    warning = function(w) {
      print(paste("WARNING:", w))
    }
  )
}

#' Peek MySQL database fields
#'
#' `mysql_peek_fields()` lists the field names present in a particular table
#' from a valid MySQL database connection
#' @seealso [mysql_connect(), mysql_peek_tables()]
#' @param cname A string representing the connection variable name from the environment
#' @param table A string representing the table name for which to list fields
#' @returns A vector of table field names from the database connection
#' @examples
#' mysql_peek_fields("mycname", "mytablename")
#'
#' # If connection was made with default cname from `mysql_connect()`, it may be called
#' # with only the "table" parameter
#' mysql_peek_fields(table = "mytablename")
#' @import DBI
#' @export
mysql_peek_fields <- function(cname = NULL, table = NULL) {
  if(is.null(cname) & !exists("con")) {
    stop("Enter a valid connection name or setup a default connection via `mysql_connect()`")
  }

  if(!is.null(cname) & !exists(cname)) {
    stop(paste0("Cannot find the connection `", cname, "` in the environment."))
  }

  if(is.null(table)) {
    stop("A valid table name must be provided. Use `mysql_peek_tables()` for a reference.")
  }

  connection = ifelse(is.null(cname), "con", cname)

  tryCatch(
    {
      DBI::dbListFields(get(connection), table)
    },
    error = function(e) {
      print(paste("ERROR:", e))
    },
    warning = function(w) {
      print(paste("WARNING:", w))
    }
  )
}

#' Send MySQL query
#'
#' `mysql_send_query()` sends a query to the database per the connection passed,
#' closes the connection when appropriate, and returns the result
#' @seealso [mysql_connect(), mysql_peek_tables(), mysql_peek_fields()]
#' @param cname A string representing the connection variable name from the environment
#' @param query A string representing the query to execute
#' @returns A dataframe of the query results
#' @examples
#' mysql_send_query("mycname", "SELECT * FROM mytable")
#' @import DBI
#' @export
mysql_send_query <- function(cname = NULL, query = NULL) {
  if(is.null(cname) & !exists("con")) {
    stop("Enter a valid connection name or setup a default connection via `mysql_connect()`")
  }

  if(!is.null(cname) & !exists(cname)) {
    stop(paste0("Cannot find the connection `", cname, "` in the environment."))
  }

  if(is.null(query)) {
    stop("A valid query must be passed.")
  }

  connection = ifelse(is.null(cname), "con", cname)

  tryCatch(
    {
      response <- DBI::dbSendQuery(get(connection), query)
      results <- DBI::dbFetch(response)

      DBI::dbClearResult(response)

      results
    },
    error = function(e) {
      print(paste("ERROR:", e))
    },
    warning = function(w) {
      print(paste("WARNING:", w))
    }
  )
}

#' Close MySQL connection
#'
#' `mysql_close()` closes an open MySQL server connection
#' @seealso [mysql_connect()]
#' @param cname A string representing the connection variable name from the environment
#' @returns Nothing
#' @examples
#' mysql_close("mycname")
#' @import DBI
#' @export
mysql_close <- function(cname = NULL) {
  if(is.null(cname) & !exists("con")) {
    stop("Enter a valid connection name or setup a default connection via `mysql_connect()`")
  }

  if(!is.null(cname) & !exists(cname)) {
    stop(paste0("Cannot find the connection `", cname, "` in the environment."))
  }

  connection = ifelse(is.null(cname), "con", cname)

  tryCatch(
    {
      DBI::dbDisconnect(get(connection))
      print("Disconnected successfully")
    },
    error <- function(e) {
      print(paste("ERROR:", e))
    },
    warning <- function(w) {
      print(paste("WARNING:", w))
    }
  )
}
