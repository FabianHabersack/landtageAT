\name{list_state_elections}
\alias{list_state_elections}
\title{List state election metadata used for legislative-period assignment}
\usage{
list_state_elections(state = NULL, force_refresh = FALSE)
}
\arguments{
  \item{state}{Optional state code or name to filter election records.}

  \item{force_refresh}{Whether to force a fresh fetch from wahldatenbank.at.}
}
\value{
A tibble with election IDs, dates, totals and party vote results.
}
\description{
Fetches Landtagswahl records from \url{https://www.wahldatenbank.at}
and returns the election metadata used internally to map protocol
\code{session_date} values to \code{legislative_period}.
}
