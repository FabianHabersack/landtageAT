\name{list_protocols}
\alias{list_protocols}
\title{List plenary protocol documents for one state}
\usage{list_protocols(state, limit = NULL, crawl_depth = NULL)}
\arguments{
  \item{state}{State code or name.}
  \item{limit}{Optional maximum number of rows.}
  \item{crawl_depth}{Optional crawler depth override for recursive backends (e.g. Wien).}
}
\value{A harmonized protocol-level tibble including provenance fields.}
\description{Returns protocol links and metadata from state-specific backends.}
