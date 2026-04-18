collect_bgld_protocols <- function() {
  urls <- c(
    "https://www.bgld-landtag.at/der-landtag/archiv/xix-gp-protokolle",
    "https://www.bgld-landtag.at/der-landtag/archiv/xx-gp-protokolle",
    "https://www.bgld-landtag.at/der-landtag/archiv/xxi-gp-protokolle"
  )

  purrr::map_dfr(urls, function(url) {
    doc <- safe_fetch_html(url)
    if (is.null(doc)) return(links_to_protocols(empty_links_tbl(), "bgld", url, backend = "bgld"))

    p_nodes <- rvest::html_elements(doc, "p")
    text <- rvest::html_text2(p_nodes)
    href <- rvest::html_attr(rvest::html_element(p_nodes, "a[href*=\".pdf\"]"), "href")

    links <- tibble::tibble(text = text, href = href) |>
      dplyr::filter(!is.na(.data$href), stringr::str_detect(tolower(.data$text), "wortprotokoll")) |>
      dplyr::mutate(
        url = xml2::url_absolute(.data$href, url),
        text = stringr::str_squish(stringr::str_remove(.data$text, "\\(pdf[^\\)]*\\)$"))
      )

    links_to_protocols(links, state = "bgld", source_url = url, backend = "bgld")
  })
}

collect_stm_protocols <- function() {
  stm_urls <- c(
    "https://www.landtag.steiermark.at/cms/ziel/181952035",
    "https://www.landtag.steiermark.at/cms/ziel/155295847",
    "https://www.landtag.steiermark.at/cms/ziel/181952052",
    "https://www.landtag.steiermark.at/cms/ziel/181952069",
    "https://www.landtag.steiermark.at/cms/ziel/181952086",
    "https://www.landtag.steiermark.at/cms/ziel/181952103",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12238146/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12233686/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12233147/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083714/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083713/111932803",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083711/112186624",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083710/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083709/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083708/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083707/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083705/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083704/111932290",
    "https://www.landesarchiv.steiermark.at/cms/beitrag/12083702/111932290"
  )

  purrr::map_dfr(seq_along(stm_urls), function(i) {
    url <- stm_urls[[i]]
    period <- as.character(20 - i)
    doc <- safe_fetch_html(url)
    if (is.null(doc)) return(links_to_protocols(empty_links_tbl(), "stm", url, backend = "stm"))

    links <- extract_links(doc, url) |>
      dplyr::filter(
        stringr::str_detect(tolower(.data$url), "\\.pdf($|\\?)"),
        stringr::str_detect(tolower(paste(.data$text, .data$url)), "protokoll|stenograf")
      ) |>
      dplyr::mutate(text = dplyr::if_else(.data$text == "", basename(.data$url), .data$text))

    links_to_protocols(links, state = "stm", source_url = url, backend = "stm") |>
      dplyr::mutate(legislative_period = period)
  })
}

collect_wie_protocols <- function() {
  years <- 1998:as.integer(format(Sys.Date(), "%Y"))
  urls <- sprintf("https://www.wien.gv.at/mdb/ltg/%s/index.htm", years)

  purrr::map_dfr(urls, function(url) {
    doc <- safe_fetch_html(url)
    if (is.null(doc)) return(links_to_protocols(empty_links_tbl(), "wie", url, backend = "wie"))

    session_nodes <- rvest::html_elements(doc, "li > strong")
    if (length(session_nodes) == 0) return(links_to_protocols(empty_links_tbl(), "wie", url, backend = "wie"))

    out <- purrr::map_dfr(session_nodes, function(sn) {
      li <- xml2::xml_parent(sn)
      session_title <- stringr::str_squish(rvest::html_text2(sn))
      pdf_node <- rvest::html_element(li, "a[href*='-w-'][href$='.pdf']")
      if (length(pdf_node) == 0 || is.na(pdf_node)) return(tibble::tibble())

      href <- rvest::html_attr(pdf_node, "href")
      full <- xml2::url_absolute(href, url)
      tibble::tibble(
        text = paste0("Wortprotokoll ", session_title),
        href = href,
        url = full
      )
    })

    links_to_protocols(out, state = "wie", source_url = url, backend = "wie")
  })
}

vbg_extract_pdf_url <- function(document_url) {
  txt <- tryCatch(
      httr2::request(document_url) |>
      httr2::req_user_agent("landtageAT/0.2.0") |>
      httr2::req_timeout(10) |>
      httr2::req_perform() |>
      httr2::resp_body_string(),
    error = function(e) ""
  )

  if (txt == "") return(NA_character_)

  m1 <- stringr::str_match(txt, 'OpenPDF\\("([^"]+)"\\)')[, 2]
  m2 <- stringr::str_match(txt, 'iframe[^>]+src="([^"]+\\.pdf[^"]*)"')[, 2]
  out <- dplyr::coalesce(m1, m2)
  if (is.na(out)) return(NA_character_)
  xml2::url_absolute(out, document_url)
}

collect_vbg_protocols <- function(max_pages = 250, max_results = NULL) {
  base <- "https://suche.vorarlberg.at/VLR/vlr_gov.nsf/alldocs_byDateDSC?SearchView"
  query <- 'FIELD fd_TypeOfDocumentTX Contains "Protokoll"'
  count <- 100L

  out <- tibble::tibble()

  for (page in seq_len(max_pages)) {
    start <- (page - 1L) * count + 1L
    url <- paste0(
      base,
      "&SearchMax=0",
      "&Count=", count,
      "&Start=", start,
      "&SearchWV=FALSE",
      "&SearchFuzzy=FALSE",
      "&SearchOrder=4",
      "&Query=", utils::URLencode(query, reserved = TRUE)
    )

    doc <- safe_fetch_html(url)
    links <- extract_links(doc, url) |>
      dplyr::filter(stringr::str_detect(tolower(.data$url), "opendocument"))

    if (nrow(links) == 0) break

    if (!is.null(max_results)) {
      remaining <- max_results - nrow(out)
      if (remaining <= 0) break
      links <- dplyr::slice_head(links, n = remaining)
    }

    page_tbl <- links_to_protocols(links, state = "vbg", source_url = url, backend = "vbg-lis") |>
      dplyr::mutate(
        source_document_url = .data$protocol_url,
        protocol_url = vapply(.data$protocol_url, vbg_extract_pdf_url, FUN.VALUE = character(1)),
        protocol_url = dplyr::if_else(is.na(.data$protocol_url) | .data$protocol_url == "", .data$source_document_url, .data$protocol_url),
        document_type = dplyr::if_else(stringr::str_detect(tolower(.data$protocol_url), "\\.pdf"), "pdf", "html")
      ) |>
      dplyr::select(-"source_document_url")

    out <- dplyr::bind_rows(out, page_tbl)

    if (!is.null(max_results) && nrow(out) >= max_results) break

    if (nrow(links) < count) break
  }

  dplyr::distinct(out, .data$protocol_url, .keep_all = TRUE)
}

collect_tir_protocols <- function() {
  url <- "https://lte.tirol.gv.at/public/sitzung/landtag/landtagsSitzungList.xhtml"
  doc <- safe_fetch_html(url)
  links <- extract_links(doc, url) |>
    dplyr::filter(stringr::str_detect(tolower(paste(.data$text, .data$url)), "sitzung|protokoll|landtag"))
  links_to_protocols(links, state = "tir", source_url = url, backend = "tir")
}

collect_ooe_protocols <- function() {
  url <- "https://www.land-oberoesterreich.gv.at/ltgspsuche.htm"
  doc <- safe_fetch_html(url)
  links <- extract_links(doc, url) |>
    dplyr::filter(stringr::str_detect(tolower(paste(.data$text, .data$url)), "protokoll|sitzung|stenograph|\\.pdf"))
  links_to_protocols(links, state = "ooe", source_url = url, backend = "ooe")
}

collect_sbg_protocols <- function() {
  url <- paste0(
    "https://service.salzburg.gv.at/lpi/searchExtern?datumVon=01.01.1994&datumBis=",
    format(Sys.Date(), "%d.%m.%Y"),
    "&artId=3&fraktionId=&periode=&session=&beilage=&titel=&text=&search="
  )
  doc <- safe_fetch_html(url)
  links <- extract_links(doc, url) |>
    dplyr::filter(stringr::str_detect(tolower(paste(.data$text, .data$url)), "protokoll|sitzung|\\.pdf"))
  links_to_protocols(links, state = "sbg", source_url = url, backend = "sbg")
}
