% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n2000_get.R
\name{n2000_getDefinitions}
\alias{n2000_getDefinitions}
\title{Download Natura 2000 definitions}
\usage{
n2000_getDefinitions(
  folder,
  year = "2019",
  force = FALSE,
 
    nlink = "https://www.eea.europa.eu/data-and-maps/data/natura-11/table-definitions/table-definitions-xls-file/at_download/file"
)
}
\arguments{
\item{folder}{A [`character`] directory path to download the Natura 2000 data definitions to. Automatically checks to see if the data already exist in the folder (can be overridden by specifying `force = T`).}

\item{year}{(optional). The [`character`] year version of the Natura 2000 data set to check if it already exists. Defaults to the most recently available version (2019).}

\item{force}{(optional). [`logical`] should a new download be forced even if data already exists in the directory? Defaults to `FALSE`.}

\item{nlink}{(normally not altered). The [`character`] direct download link for Natura 2000 definitions file.}
}
\description{
Download Natura 2000 definitions
}
\author{
Matt Lewis

Martin Jung
}
