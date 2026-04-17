\name{download_protocols}
\alias{download_protocols}
\title{Download protocol documents}
\usage{download_protocols(protocols, destdir = tempdir(), overwrite = FALSE)}
\arguments{
  \item{protocols}{A tibble returned by \code{list_protocols()}.}
  \item{destdir}{Destination directory.}
  \item{overwrite}{Whether existing files should be overwritten.}
}
\value{Input tibble enriched with local file path and download status columns.}
\description{Downloads protocol files from discovered URLs and records status metadata.}
