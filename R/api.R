#' List supported Austrian state parliaments
#'
#' @return A tibble with state codes, names and entry URLs.
#' @export
list_states <- function() {
  .state_catalog
}

#' Report implemented feature support by state
#'
#' @return A tibble indicating supported features in this package version.
#' @export
landtage_supported_features <- function() {
  dplyr::left_join(.state_catalog, .backend_feature_matrix, by = "state")
}

collect_state_protocols <- function(state_code, crawl_depth = NULL) {
  custom <- switch(
    state_code,
    bgld = collect_bgld_protocols(),
    stm = collect_stm_protocols(),
    wie = collect_wie_protocols(),
    vbg = collect_vbg_protocols(),
    tir = collect_tir_protocols(),
    ooe = collect_ooe_protocols(),
    sbg = collect_sbg_protocols(),
    NULL
  )

  if (!is.null(custom)) {
    return(
      custom |>
        dplyr::mutate(legislative_period = normalize_legislative_period(.data$legislative_period)) |>
        dplyr::distinct(.data$protocol_url, .keep_all = TRUE)
    )
  }

  cfg <- state_backend(state_code)
  urls <- state_entry_urls(state_code)
  if (is.null(crawl_depth)) crawl_depth <- cfg$crawl_depth_default[[1]]

  out <- purrr::map_dfr(urls, function(url) {
    seed_links <- extract_links(safe_fetch_html(url), url)
    if (nrow(seed_links) == 0) {
      return(tibble::tibble(
        state = state_code,
        state_name = state_name(state_code),
        session_id = NA_character_,
        session_date = as.Date(NA),
        title = NA_character_,
        legislative_period = source_period_hint(state_code, url),
        protocol_url = NA_character_,
        document_type = NA_character_,
        source_url = url,
        backend = "html",
        scraped_at = Sys.time()
      ))
    }

    filtered_seed <- filter_links_for_state(
      seed_links,
      include_pattern = cfg$include_pattern[[1]],
      exclude_pattern = cfg$exclude_pattern[[1]]
    )

    links <- filtered_seed
    if (isTRUE(cfg$follow_level1[[1]])) {
      followed <- follow_relevant_links(
        filtered_seed,
        include_pattern = cfg$include_pattern[[1]],
        exclude_pattern = cfg$exclude_pattern[[1]]
      )
      links <- dplyr::bind_rows(filtered_seed, followed) |>
        dplyr::distinct(.data$url, .keep_all = TRUE)
    }

    links_to_protocols(links, state = state_code, source_url = url, backend = "html") |>
      dplyr::mutate(legislative_period = dplyr::coalesce(.data$legislative_period, source_period_hint(state_code, url)))
  })

  out |>
    dplyr::filter(!is.na(.data$protocol_url), .data$protocol_url != "") |>
    dplyr::mutate(legislative_period = normalize_legislative_period(.data$legislative_period)) |>
    dplyr::distinct(.data$protocol_url, .keep_all = TRUE)
}

#' List plenary sessions for one state
#'
#' @param state State code or name.
#' @param limit Optional max number of rows.
#' @param crawl_depth Crawl depth for states needing link traversal (for `wie`; default from backend config).
#' @return Tibble with harmonized session-level metadata.
#' @export
list_sessions <- function(state, limit = NULL, crawl_depth = NULL) {
  state_code <- normalize_state(state)
  protocols <- collect_state_protocols(state_code, crawl_depth = crawl_depth)

  sessions <- protocols |>
    dplyr::transmute(
      state = .data$state,
      state_name = .data$state_name,
      session_id = .data$session_id,
      session_date = .data$session_date,
      title = .data$title,
      legislative_period = .data$legislative_period,
      source_url = .data$source_url,
      backend = .data$backend,
      scraped_at = .data$scraped_at
    ) |>
    dplyr::distinct(.data$session_id, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$session_date), .data$title)

  if (!is.null(limit)) sessions <- dplyr::slice_head(sessions, n = limit)
  sessions
}

#' List plenary protocol documents for one state
#'
#' @param state State code or name.
#' @param limit Optional max number of rows.
#' @param crawl_depth Crawl depth for states needing link traversal (for `wie`; default from backend config).
#' @return Tibble with protocol URLs and provenance metadata.
#' @export
list_protocols <- function(state, limit = NULL, crawl_depth = NULL) {
  state_code <- normalize_state(state)
  protocols <- collect_state_protocols(state_code, crawl_depth = crawl_depth) |>
    dplyr::arrange(dplyr::desc(.data$session_date), .data$title)

  if (!is.null(limit)) protocols <- dplyr::slice_head(protocols, n = limit)
  protocols
}

#' Download protocol documents from a protocol table
#'
#' @param protocols A tibble returned by [list_protocols()].
#' @param destdir Destination directory.
#' @param overwrite Whether existing files should be overwritten.
#' @return Input tibble with local file paths and download status.
#' @export
download_protocols <- function(protocols, destdir = tempdir(), overwrite = FALSE) {
  if (!all(c("protocol_url", "state") %in% names(protocols))) {
    cli::cli_abort("`protocols` must include `protocol_url` and `state` columns.")
  }

  dir.create(destdir, recursive = TRUE, showWarnings = FALSE)

  downloader <- function(url, state) {
    ext <- tolower(tools::file_ext(url))
    ext <- ifelse(ext == "", "bin", ext)
    file <- file.path(destdir, paste0(state, "_", digest_id(url), ".", ext))

    if (file.exists(file) && !overwrite) {
      return(list(path = file, status = "exists", ok = TRUE, error = NA_character_))
    }

    tryCatch({
      httr2::request(url) |>
        httr2::req_user_agent("landtageAT/0.2.0") |>
        httr2::req_timeout(60) |>
        httr2::req_perform(path = file)
      list(path = file, status = "downloaded", ok = TRUE, error = NA_character_)
    }, error = function(e) {
      list(path = NA_character_, status = "failed", ok = FALSE, error = conditionMessage(e))
    })
  }

  results <- purrr::map2(protocols$protocol_url, protocols$state, downloader)
  protocols |>
    dplyr::mutate(
      file_path = purrr::map_chr(results, "path"),
      download_status = purrr::map_chr(results, "status"),
      download_ok = purrr::map_lgl(results, "ok"),
      download_error = purrr::map_chr(results, "error")
    )
}

#' Extract textual content from protocol files
#'
#' @param x Local file path or URL.
#' @param max_pages Maximum PDF pages to parse.
#' @return Tibble with document and page-level text.
#' @export
extract_protocol_text <- function(x, max_pages = Inf) {
  local_path <- x
  cleanup <- FALSE

  if (grepl("^https?://", x)) {
    tmp <- tempfile(fileext = paste0(".", tools::file_ext(x)))
    httr2::request(x) |>
      httr2::req_user_agent("landtageAT/0.2.0") |>
      httr2::req_timeout(60) |>
      httr2::req_perform(path = tmp)
    local_path <- tmp
    cleanup <- TRUE
  }

  on.exit(if (cleanup && file.exists(local_path)) file.remove(local_path), add = TRUE)

  ext <- tolower(tools::file_ext(local_path))
  if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      cli::cli_abort("Package `pdftools` is required to extract PDF text. Install it with `install.packages(\"pdftools\")`.")
    }
    txt <- pdftools::pdf_text(local_path)
    if (is.finite(max_pages)) txt <- txt[seq_len(min(length(txt), max_pages))]
    return(tibble::tibble(
      source = x,
      file_path = normalizePath(local_path, mustWork = FALSE),
      page = seq_along(txt),
      text = txt
    ))
  }

  if (ext %in% c("html", "htm")) {
    doc <- xml2::read_html(local_path)
    txt <- rvest::html_text2(rvest::html_element(doc, "body"))
    return(tibble::tibble(source = x, file_path = normalizePath(local_path, mustWork = FALSE), page = 1L, text = txt))
  }

  cli::cli_abort("Unsupported file format `{ext}` for text extraction.")
}

#' List MPs/member metadata (currently limited support)
#'
#' @param state State code or name.
#' @return Tibble with harmonized columns and feature availability note.
#' @export
list_members <- function(state) {
  state_code <- normalize_state(state)
  tibble::tibble(
    state = state_code,
    state_name = state_name(state_code),
    member_id = NA_character_,
    member_name = NA_character_,
    party = NA_character_,
    valid_from = as.Date(NA),
    valid_to = as.Date(NA),
    source_url = state_entry_urls(state_code)[1],
    backend = "not_yet_implemented",
    note = "Member-level harmonized extraction is planned; backend scaffolding is stable."
  )
}

digest_id <- function(x) {
  raw <- charToRaw(enc2utf8(x))
  paste(sprintf("%02x", as.integer(raw)[seq_len(min(length(raw), 8))]), collapse = "")
}
