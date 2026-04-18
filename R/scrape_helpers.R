empty_links_tbl <- function() {
  tibble::tibble(text = character(), href = character(), url = character())
}

fetch_html <- function(url, user_agent = "landtageAT/0.2.0") {
  req <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_timeout(30)
  resp <- httr2::req_perform(req)
  raw <- httr2::resp_body_raw(resp)

  tryCatch(
    xml2::read_html(raw),
    error = function(e) {
      txt <- rawToChar(raw)
      txt_utf8 <- iconv(txt, from = "latin1", to = "UTF-8", sub = "")
      xml2::read_html(txt_utf8)
    }
  )
}

safe_fetch_html <- function(url) {
  tryCatch(fetch_html(url), error = function(e) NULL)
}

null_or <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

election_state_map <- function(x) {
  key <- tolower(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  dplyr::recode(
    key,
    "burgenland" = "bgld",
    "karnten" = "ktn",
    "niederosterreich" = "noe",
    "oberosterreich" = "ooe",
    "salzburg" = "sbg",
    "steiermark" = "stm",
    "tirol" = "tir",
    "vorarlberg" = "vbg",
    "wien" = "wie",
    .default = NA_character_
  )
}

.election_cache <- new.env(parent = emptyenv())

fetch_landtag_elections <- function(force_refresh = FALSE) {
  if (!force_refresh && !is.null(.election_cache$data)) return(.election_cache$data)

  basics <- httr2::request("https://www.wahldatenbank.at/basics.json") |>
    httr2::req_user_agent("landtageAT/0.2.0") |>
    httr2::req_timeout(30) |>
    httr2::req_perform() |>
    httr2::resp_body_json(check_type = FALSE)

  wahlen <- basics$wahlen
  ids <- names(wahlen)
  names_vec <- vapply(wahlen, function(x) null_or(x$name, NA_character_), FUN.VALUE = character(1))
  levels_vec <- vapply(wahlen, function(x) paste(null_or(x$levels, character()), collapse = ","), FUN.VALUE = character(1))

  candidates <- tibble::tibble(
    election_id = ids,
    election_name = names_vec,
    levels = levels_vec
  ) |>
    dplyr::filter(
      stringr::str_detect(.data$election_name, "^Landtagswahl\\s+"),
      stringr::str_detect(.data$levels, "Bundesland")
    ) |>
    dplyr::mutate(
      state_name = stringr::str_trim(stringr::str_remove(.data$election_name, "^Landtagswahl\\s+")),
      state_name = stringr::str_trim(stringr::str_remove(.data$state_name, "\\s+\\d{4}$")),
      state = election_state_map(.data$state_name)
    ) |>
    dplyr::filter(!is.na(.data$state))

  out <- purrr::map_dfr(seq_len(nrow(candidates)), function(i) {
    el <- candidates$election_id[[i]]
    state <- candidates$state[[i]]

    payload <- tryCatch(
      httr2::request("https://www.wahldatenbank.at/get_election.php") |>
        httr2::req_user_agent("landtageAT/0.2.0") |>
        httr2::req_timeout(30) |>
        httr2::req_url_query(el = el, lvl = "Bundesland") |>
        httr2::req_perform() |>
        httr2::resp_body_json(check_type = FALSE),
      error = function(e) NULL
    )

    if (is.null(payload) || is.null(payload$data) || length(payload$data) == 0) return(tibble::tibble())

    row <- if (is.data.frame(payload$data)) {
      as.list(payload$data[1, , drop = TRUE])
    } else {
      payload$data[[1]]
    }
    parties <- strsplit(null_or(payload$parties, ""), ",", fixed = TRUE)[[1]]
    parties <- parties[parties != ""]
    party_votes <- stats::setNames(as.list(unname(unlist(row[parties]))), parties)

    tibble::tibble(
      state = state,
      election_id = el,
      election_name = null_or(payload$name, candidates$election_name[[i]]),
      election_date = as.Date(null_or(payload$date, NA_character_)),
      election_eligible = as.numeric(null_or(row$eligible, NA_real_)),
      election_votes = as.numeric(null_or(row$votes, NA_real_)),
      election_valid = as.numeric(null_or(row$valid, NA_real_)),
      election_invalid = as.numeric(null_or(row$invalid, NA_real_)),
      election_party_results = list(party_votes)
    )
  }) |>
    dplyr::arrange(.data$state, .data$election_date) |>
    dplyr::group_by(.data$state) |>
    dplyr::mutate(legislative_period = as.character(dplyr::row_number())) |>
    dplyr::ungroup()

  .election_cache$data <- out
  out
}

enrich_with_elections <- function(protocols) {
  if (nrow(protocols) == 0) return(protocols)

  elections <- fetch_landtag_elections()
  if (nrow(elections) == 0) return(protocols)

  split_protocols <- split(protocols, protocols$state)
  enriched <- purrr::map_dfr(split_protocols, function(df_state) {
    st <- df_state$state[[1]]
    e <- dplyr::filter(elections, .data$state == st) |>
      dplyr::arrange(.data$election_date)

    if (nrow(e) == 0) {
      return(df_state |>
        dplyr::mutate(
          legislative_period = NA_character_,
          election_id = NA_character_,
          election_name = NA_character_,
          election_date = as.Date(NA),
          election_eligible = NA_real_,
          election_votes = NA_real_,
          election_valid = NA_real_,
          election_invalid = NA_real_,
          election_party_results = vector("list", dplyr::n())
        ))
    }

    idx <- findInterval(df_state$session_date, e$election_date)
    has_match <- !is.na(idx) & idx > 0
    idx_safe <- pmax(idx, 1)

    df_state |>
      dplyr::mutate(
        legislative_period = dplyr::if_else(has_match, e$legislative_period[idx_safe], NA_character_),
        election_id = dplyr::if_else(has_match, e$election_id[idx_safe], NA_character_),
        election_name = dplyr::if_else(has_match, e$election_name[idx_safe], NA_character_),
        election_date = dplyr::if_else(has_match, e$election_date[idx_safe], as.Date(NA)),
        election_eligible = dplyr::if_else(has_match, e$election_eligible[idx_safe], NA_real_),
        election_votes = dplyr::if_else(has_match, e$election_votes[idx_safe], NA_real_),
        election_valid = dplyr::if_else(has_match, e$election_valid[idx_safe], NA_real_),
        election_invalid = dplyr::if_else(has_match, e$election_invalid[idx_safe], NA_real_),
        election_party_results = purrr::map(seq_len(dplyr::n()), function(i) {
          if (!has_match[[i]]) return(list())
          e$election_party_results[[idx_safe[[i]]]]
        })
      )
  })

  dplyr::bind_rows(enriched)
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
    dplyr::filter(!stringr::str_detect(tolower(.data$href), "^(javascript:|mailto:|tel:|data:|#)")) |>
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

infer_legislative_period <- function(x) {
  txt <- tolower(x)

  gp_roman_prefix <- stringr::str_extract(txt, "(?:gp[-_ ]?|gesetzgebungsperiode[-_ ]?)([xivlcdm]{1,8})")
  gp_roman_prefix <- stringr::str_replace(gp_roman_prefix, "(?:gp[-_ ]?|gesetzgebungsperiode[-_ ]?)", "")

  gp_roman_suffix <- stringr::str_extract(txt, "([xivlcdm]{1,8})[-_ ]?gp")
  gp_roman_suffix <- stringr::str_replace(gp_roman_suffix, "[-_ ]?gp", "")

  gp_arabic <- stringr::str_extract(txt, "\\b\\d{1,2}\\.?\\s*gesetzgebungsperiode")
  gp_arabic <- stringr::str_extract(gp_arabic, "\\d{1,2}")

  slash_roman <- stringr::str_extract(txt, "/sitzungen/([xivlcdm]{1,8})")
  slash_roman <- stringr::str_replace(slash_roman, "/sitzungen/", "")

  lt_roman <- stringr::str_extract(txt, "([xivlcdm]{1,8})\\.?\\s*landtagsperiode")
  lt_roman <- stringr::str_extract(lt_roman, "[xivlcdm]{1,8}")

  out <- dplyr::coalesce(gp_roman_prefix, gp_roman_suffix, gp_arabic, slash_roman, lt_roman)
  toupper(out)
}


normalize_legislative_period <- function(x) {
  if (length(x) == 0) return(character())
  clean <- stringr::str_squish(toupper(as.character(x)))
  clean[clean %in% c("", "NA", "N/A")] <- NA_character_

  out <- clean
  is_roman <- !is.na(clean) & stringr::str_detect(clean, "^[IVXLCDM]+$")
  if (any(is_roman)) {
    out[is_roman] <- as.character(suppressWarnings(as.numeric(as.roman(clean[is_roman]))))
  }

  # keep arabic as-is
  is_num <- !is.na(clean) & stringr::str_detect(clean, "^\\d+$")
  out[is_num] <- clean[is_num]
  out
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


safe_url_basename <- function(url) {
  if (is.na(url) || url == "") return(NA_character_)
  clean <- strsplit(url, "?", fixed = TRUE)[[1]][1]
  out <- suppressWarnings(tryCatch(basename(clean), error = function(e) NA_character_))
  if (is.na(out) || out == "" || nchar(out) > 180) {
    short <- substr(gsub("[^A-Za-z0-9]+", "_", clean), 1, 80)
    if (short == "") short <- "document"
    out <- short
  }
  out
}

links_to_protocols <- function(links, state, source_url, backend = "html") {
  if (nrow(links) == 0) {
    return(tibble::tibble(
      state = character(),
      state_name = character(),
      session_id = character(),
      session_date = as.Date(character()),
      title = character(),
      legislative_period = character(),
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
      legislative_period = infer_legislative_period(paste(.data$text, .data$url, source_url)),
      title = dplyr::if_else(.data$text == "", vapply(.data$url, safe_url_basename, FUN.VALUE = character(1)), .data$text),
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
      legislative_period = normalize_legislative_period(.data$legislative_period),
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
