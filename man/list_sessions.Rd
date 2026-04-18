\name{list_sessions}
\alias{list_sessions}
\title{List plenary sessions for one state}
\usage{list_sessions(state, limit = NULL, crawl_depth = NULL)}
\arguments{
  \item{state}{State code or name.}
  \item{limit}{Optional maximum number of rows.}
  \item{crawl_depth}{Optional crawler depth override for recursive backends (e.g. Wien).}
}
\value{A harmonized session-level tibble.}
\description{Discovers state-specific parliamentary session/protocol references and returns session-level metadata.}
