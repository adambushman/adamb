% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql.R
\name{mysql_connect}
\alias{mysql_connect}
\title{Establish MySQL connection}
\usage{
mysql_connect(
  username = NULL,
  password = NULL,
  dbname = NULL,
  host = NULL,
  port = NULL,
  cname = NULL
)
}
\arguments{
\item{username}{A string representing the user's name}

\item{password}{A string representing the user's password}

\item{dbname}{A string representing the database name to which to connect}

\item{host}{A string representing the server host for the database; defaults to
common DigitalOcean host}

\item{port}{An integer representing the server port for the database; defaults to
common DigitalOcean port}

\item{cname}{A string representing the connection variable name for the environment;
defaults to "con"}
}
\value{
A connection object for use with additional MySQL functions from the package
}
\description{
`mysql_connect()` creates a connection with a MySQL database
}
\examples{
mysql_connect("keychain_user", "keychain_pass", "defaultdb")

# Pass custom connection details
mysql_connect("keychain_user", "keychain_pass", "defaultdb", "myhost", "myport", "mycname")
}
