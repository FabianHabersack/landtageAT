empty_links_tbl <- function() {
  tibble::tibble(text = character(), href = character(), url = character())
}

fetch_html <- function(url, user_agent = "landtageAT/0.2.0") {
  req <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_timeout(30)
  resp <- httr2::req_perform(req)
  xml2::read_html(httr2::resp_body_string(resp))
}

safe_fetch_html <- function(url) {
  tryCatch(fetch_html(url), error = function(e) NULL)
}

extract_links <- function(doc, base_url, pattern = NULL) {
  if (is.null(doc)) return(empty_links_tbl())
  href_nodes <- rvest::html_elements(doc, "a")
  if (length(href_nodes) == 0) return(empty_links_tbl())

  out <- tibble::tibble(
    text = rvest::html_text2(href_nodes),
    href = rvest::html_attr(href_nodes, "href")
  ) |>
    dplyr::filter(!is.na(.data$href), .data$href != "") |>
    dplyr::mutate(
      url = xml2::url_absolute(.data$href, base = base_url),
      text = stringr::str_squish(.data$text)
    ) |>
    dplyr::filter(!is.na(.data$url), .data$url != "")

  if (!is.null(pattern)) {
    out <- dplyr::filter(out, stringr::str_detect(tolower(.data$url), tolower(pattern)))
  }

  dplyr::distinct(out, .data$url, .keep_all = TRUE)
}

infer_date <- function(x) {
  y <- stringr::str_extract(x, "\\d{4}-\\d{2}-\\d{2}|\\d{2}\\.\\d{2}\\.\\d{4}|\\d{1,2}\\.\\d{1,2}\\.\\d{4}")
  y <- dplyr::case_when(
    is.na(y) ~ NA_character_,
    stringr::str_detect(y, "^\\d{4}-") ~ y,
    TRUE ~ gsub("\\.", "-", y)
  )
  out <- suppressWarnings(as.Date(y, format = "%d-%m-%Y"))
  needs_iso <- is.na(out) & !is.na(y)
  out[needs_iso] <- suppressWarnings(as.Date(y[needs_iso]))
  out
}

is_document_url <- function(url) {
  stringr::str_detect(tolower(url), "\\.(pdf|doc|docx|rtf|odt|txt)($|\\?)")
}

is_html_url <- function(url) {
  stringr::str_detect(tolower(url), "^https?://") &
    !is_document_url(url) &
    !stringr::str_detect(tolower(url), "\\.(jpg|jpeg|png|gif|zip|mp3|mp4)($|\\?)")
}

filter_links_for_state <- function(links, include_pattern, exclude_pattern = NULL) {
  if (nrow(links) == 0) return(links)
  out <- dplyr::filter(links, stringr::str_detect(tolower(paste(.data$text, .data$url)), tolower(include_pattern)))
  if (!is.null(exclude_pattern)) {
    out <- dplyr::filter(out, !stringr::str_detect(tolower(paste(.data$text, .data$url)), tolower(exclude_pattern)))
  }
  dplyr::distinct(out, .data$url, .keep_all = TRUE)
}

follow_relevant_links <- function(seed_links, include_pattern, exclude_pattern = NULL) {
  if (nrow(seed_links) == 0) return(empty_links_tbl())
  candidates <- dplyr::filter(seed_links, is_html_url(.data$url))
  if (nrow(candidates) == 0) return(empty_links_tbl())

  discovered <- purrr::map_dfr(candidates$url, function(u) {
    doc <- safe_fetch_html(u)
    links <- extract_links(doc, u)
    filter_links_for_state(links, include_pattern = include_pattern, exclude_pattern = exclude_pattern)
  })

  dplyr::distinct(discovered, .data$url, .keep_all = TRUE)
}

links_to_protocols <- function(links, state, source_url, backend = "html") {
  if (nrow(links) == 0) {
    return(tibble::tibble(
      state = character(),
      state_name = character(),
      session_id = character(),
      session_date = as.Date(character()),
      title = character(),
      protocol_url = character(),
      document_type = character(),
      source_url = character(),
      backend = character(),
      scraped_at = as.POSIXct(character())
    ))
  }

  links |>
    dplyr::mutate(
      session_date = infer_date(paste(.data$text, .data$url)),
      title = dplyr::if_else(.data$text == "", basename(.data$url), .data$text),
      document_type = dplyr::case_when(
        stringr::str_detect(tolower(.data$url), "\\.pdf($|\\?)") ~ "pdf",
        stringr::str_detect(tolower(.data$url), "\\.docx?($|\\?)") ~ "doc",
        stringr::str_detect(tolower(.data$url), "\\.rtf($|\\?)") ~ "rtf",
        TRUE ~ "html"
      ),
      session_id = dplyr::coalesce(
        stringr::str_extract(.data$url, "(Sitzung|Sitz|TOP|ltg)[-_ ]?\\d{1,4}"),
        paste0(state, "-", dplyr::row_number())
      )
    ) |>
    dplyr::transmute(
      state = state,
      state_name = state_name(state),
      session_id = .data$session_id,
      session_date = .data$session_date,
      title = .data$title,
      protocol_url = .data$url,
      document_type = .data$document_type,
      source_url = source_url,
      backend = backend,
      scraped_at = Sys.time()
    )
}

crawl_for_pdfs <- function(seed_url, max_depth = 1) {
  visited <- character()
  queue <- list(list(url = seed_url, depth = 0))
  found <- empty_links_tbl()

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    if (current$url %in% visited) next
    visited <- c(visited, current$url)

    doc <- safe_fetch_html(current$url)
    links <- extract_links(doc, current$url)
    if (nrow(links) == 0) next

    pdf_links <- dplyr::filter(links, is_document_url(.data$url))
    found <- dplyr::bind_rows(found, pdf_links)

    if (current$depth < max_depth) {
      html_links <- dplyr::filter(links, is_html_url(.data$url))
      queue <- c(queue, purrr::map(html_links$url, ~list(url = .x, depth = current$depth + 1)))
    }
  }

  dplyr::distinct(found, .data$url, .keep_all = TRUE)
}
