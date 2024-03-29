% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IR_db_archive.R
\name{archive_IRdb}
\alias{archive_IRdb}
\title{archive_IRdb}
\usage{
archive_IRdb(IR_cycle, db)
}
\arguments{
\item{IR_cycle}{IR cycle to be archived. Should be a 4 digit year. YYYY}

\item{db}{IR database connection name}
}
\value{
Returns nothing. Creates new tables in IR database
}
\description{
Archives previous cycle's IR database
This function archives the finished IR cycle in the IR database. This function should be run at the end of the IR
Cycle before any data is imported into a new IR cycle.
}
