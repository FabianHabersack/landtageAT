\name{extract_protocol_text}
\alias{extract_protocol_text}
\title{Extract textual content from protocol files}
\usage{extract_protocol_text(x, max_pages = Inf)}
\arguments{
  \item{x}{Local file path or URL.}
  \item{max_pages}{Maximum number of PDF pages to extract when applicable.}
}
\value{A tibble with source, file path, page and extracted text.}
\description{Extracts text from PDF and HTML protocol documents.}
