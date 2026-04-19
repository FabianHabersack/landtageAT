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

collect_ktn_protocols <- function() {
  url <- "https://www.ktn.gv.at/Politik/Landtag/Stenographische-Protokolle"
  doc <- safe_fetch_html(url)
  links <- extract_links(doc, url) |>
    dplyr::filter(
      stringr::str_detect(tolower(paste(.data$text, .data$url)), "stenograph|protokoll|wortprotokoll|sitzung"),
      !stringr::str_detect(tolower(paste(.data$text, .data$url)), "besucher|mediathek|kontakt|impressum")
    )
  links_to_protocols(links, state = "ktn", source_url = url, backend = "ktn")
}

collect_noe_protocols <- function() {
  url <- "https://noe-landtag.gv.at/sitzungen"
  doc <- safe_fetch_html(url)
  session_pages <- extract_links(doc, url) |>
    dplyr::filter(stringr::str_detect(tolower(.data$url), "/veranstaltungen/")) |>
    dplyr::distinct(.data$url) |>
    dplyr::pull(.data$url)

  nested <- purrr::map_dfr(session_pages, function(u) {
    d <- safe_fetch_html(u)
    extract_links(d, u)
  })

  links <- nested |>
    dplyr::filter(
      stringr::str_detect(tolower(paste(.data$text, .data$url)), "protokoll|wortprotokoll|stenograph|\\.pdf|\\.doc"),
      !stringr::str_detect(tolower(paste(.data$text, .data$url)), "sitzplan|kalender|in kalender speichern|\\.ics")
    )
  links_to_protocols(links, state = "noe", source_url = url, backend = "noe")
}

collect_wie_protocols <- function() {
  overview_url <- "https://www.wien.gv.at/mdb/ltg/"
  overview_doc <- safe_fetch_html(overview_url)

  urls <- if (!is.null(overview_doc)) {
    extract_links(overview_doc, overview_url) |>
      dplyr::filter(stringr::str_detect(.data$url, "/mdb/ltg/\\d{4}/?(index\\.htm)?$")) |>
      dplyr::mutate(url = stringr::str_replace(.data$url, "/$", "/index.htm")) |>
      dplyr::distinct(.data$url) |>
      dplyr::arrange(dplyr::desc(.data$url)) |>
      dplyr::pull(.data$url)
  } else {
    years <- 1998:as.integer(format(Sys.Date(), "%Y"))
    sprintf("https://www.wien.gv.at/mdb/ltg/%s/index.htm", years)
  }

  purrr::map_dfr(urls, function(url) {
    doc <- safe_fetch_html(url)
    if (is.null(doc)) return(links_to_protocols(empty_links_tbl(), "wie", url, backend = "wie"))

    nodes <- rvest::html_elements(doc, "a[href]")
    if (length(nodes) == 0) return(links_to_protocols(empty_links_tbl(), "wie", url, backend = "wie"))

    session_title <- vapply(seq_along(nodes), function(i) {
      sn <- xml2::xml_find_first(nodes[[i]], "ancestor::li[strong][1]/strong[1]")
      if (inherits(sn, "xml_missing")) return(NA_character_)
      txt <- stringr::str_squish(rvest::html_text2(sn))
      if (is.na(txt) || txt == "") NA_character_ else txt
    }, FUN.VALUE = character(1))

    links <- tibble::tibble(
      text = rvest::html_text2(nodes),
      href = rvest::html_attr(nodes, "href"),
      session_title = session_title
    ) |>
      dplyr::filter(!is.na(.data$href), .data$href != "") |>
      dplyr::mutate(
        url = xml2::url_absolute(.data$href, url),
        text = stringr::str_squish(.data$text),
        text_lc = tolower(.data$text),
        url_lc = tolower(.data$url)
      ) |>
      dplyr::filter(
        stringr::str_detect(.data$url_lc, "ltg-"),
        stringr::str_detect(.data$url_lc, "\\.(htm|html)($|\\?)")
      )

    wp_links <- links |>
      dplyr::filter(stringr::str_detect(.data$text_lc, "wortprotokoll") | stringr::str_detect(.data$url_lc, "-w-"))

    if (nrow(wp_links) == 0) {
      wp_links <- links |>
        dplyr::filter(stringr::str_detect(.data$text_lc, "sitzungsbericht") | stringr::str_detect(.data$url_lc, "-s-"))
    }

    out <- wp_links |>
      dplyr::transmute(
        text = dplyr::if_else(
          !is.na(.data$session_title) & .data$session_title != "",
          paste0(dplyr::if_else(stringr::str_detect(.data$url_lc, "-w-"), "Wortprotokoll ", "Sitzungsbericht "), .data$session_title),
          dplyr::if_else(.data$text == "", basename(.data$url), .data$text)
        ),
        href = .data$href,
        url = .data$url
      )

    links_to_protocols(out, state = "wie", source_url = url, backend = "wie") |>
      dplyr::distinct(.data$protocol_url, .keep_all = TRUE)
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
  base_url <- "https://lte.tirol.gv.at/public/sitzung/landtag/landtagsSitzungList.xhtml?cid=1"

  fetch_session_page <- function(from_date = NULL, to_date = NULL) {
    cookie_file <- tempfile(fileext = ".cookies")
    first <- tryCatch(
      httr2::request(base_url) |>
        httr2::req_user_agent("landtageAT/0.2.0") |>
        httr2::req_timeout(30) |>
        httr2::req_cookie_preserve(cookie_file) |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (is.null(first)) return(list(links = empty_links_tbl(), total = 0L))

    start_doc <- xml2::read_html(httr2::resp_body_string(first))
    view_state <- rvest::html_attr(rvest::html_element(start_doc, "input[name='jakarta.faces.ViewState']"), "value")
    token <- rvest::html_attr(rvest::html_element(start_doc, "input[name='token']"), "value")
    if (is.na(view_state) || is.na(token) || view_state == "" || token == "") {
      return(list(links = empty_links_tbl(), total = 0L))
    }

    from_txt <- if (is.null(from_date)) "" else format(from_date, "%d.%m.%Y")
    to_txt <- if (is.null(to_date)) "" else format(to_date, "%d.%m.%Y")

    search_resp <- tryCatch(
      httr2::request(base_url) |>
        httr2::req_user_agent("landtageAT/0.2.0") |>
        httr2::req_timeout(30) |>
        httr2::req_cookie_preserve(cookie_file) |>
        httr2::req_body_form(
          "listContent:j_id_3n:entityComplete_input" = "",
          "listContent:j_id_3n:entityComplete_hinput" = "",
          "listContent:j_id_3q:menu_input" = "",
          "listContent:j_id_3x_input" = from_txt,
          "listContent:j_id_3y_input" = to_txt,
          "listContent:j_id_3z_input" = "",
          "listContent:j_id_44_9" = "listContent:j_id_44_9",
          "token" = token,
          "listContent:fid_SUBMIT" = "1",
          "jakarta.faces.ViewState" = view_state
        ) |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (is.null(search_resp)) return(list(links = empty_links_tbl(), total = 0L))

    result_doc <- xml2::read_html(httr2::resp_body_string(search_resp))
    links <- tibble::tibble(
      text = rvest::html_text2(rvest::html_elements(result_doc, xpath = "//*[@id='listContent:resultForm:resultTable_data']//a[@href]")),
      href = rvest::html_attr(rvest::html_elements(result_doc, xpath = "//*[@id='listContent:resultForm:resultTable_data']//a[@href]"), "href")
    ) |>
      dplyr::mutate(url = xml2::url_absolute(.data$href, base_url)) |>
      dplyr::filter(
        stringr::str_detect(.data$url, "landtagsSitzungStamm\\.xhtml\\?id="),
        !is.na(.data$url)
      ) |>
      dplyr::distinct(.data$url, .keep_all = TRUE)

    page_info <- rvest::html_text2(rvest::html_element(result_doc, "span.ui-paginator-current"))
    total <- suppressWarnings(as.integer(stringr::str_match(page_info, "von\\s+(\\d+)")[, 2]))
    if (is.na(total)) total <- nrow(links)

    list(links = links, total = total)
  }

  years <- seq.int(as.integer(format(Sys.Date(), "%Y")), 1918L, by = -1L)
  session_links <- purrr::map_dfr(years, function(y) {
    from_date <- as.Date(sprintf("%04d-01-01", y))
    to_date <- as.Date(sprintf("%04d-12-31", y))
    page <- fetch_session_page(from_date, to_date)
    page$links
  }) |>
    dplyr::distinct(.data$url, .keep_all = TRUE) |>
    dplyr::mutate(session_date = infer_date(.data$text)) |>
    dplyr::arrange(dplyr::desc(.data$session_date), dplyr::desc(.data$url)) |>
    dplyr::select(-"session_date")

  if (nrow(session_links) == 0) return(links_to_protocols(empty_links_tbl(), "tir", base_url, backend = "tir"))

  docs_links <- purrr::map_dfr(session_links$url, function(sess_url) {
    sid <- stringr::str_match(sess_url, "id=([0-9]+)")[, 2]
    if (is.na(sid)) return(empty_links_tbl())
    docs_url <- sprintf("https://lte.tirol.gv.at/public/sitzung/landtag/landtagsSitzungDokList.xhtml?id=%s&cid=1", sid)
    d <- safe_fetch_html(docs_url)
    if (is.null(d)) return(empty_links_tbl())

    rows <- rvest::html_elements(d, xpath = "//*[@id='listContent:resultForm:resultTable_data']//tr")
    if (length(rows) == 0) return(empty_links_tbl())

    tibble::tibble(
      text = rvest::html_text2(rows),
      href = vapply(rows, function(row) {
        node <- rvest::html_element(row, xpath = ".//a[contains(@href, 'landtagsSitzungDokStamm.xhtml')]")
        if (inherits(node, "xml_missing")) NA_character_ else rvest::html_attr(node, "href")
      }, FUN.VALUE = character(1))
    ) |>
      dplyr::filter(
        !is.na(.data$href),
        stringr::str_detect(tolower(.data$text), "kurzprotokoll")
      ) |>
      dplyr::mutate(
        text = stringr::str_squish(stringr::str_remove_all(.data$text, "^(pageview\\s*|file_pdf\\s*)+")),
        text = stringr::str_squish(stringr::str_replace(.data$text, "\\$\\(function\\(\\)\\{.*$", ""))
      ) |>
      dplyr::mutate(url = xml2::url_absolute(.data$href, docs_url)) |>
      dplyr::select(.data$text, .data$href, .data$url)
  })

  links_to_protocols(docs_links, state = "tir", source_url = base_url, backend = "tir") |>
    dplyr::distinct(.data$protocol_url, .keep_all = TRUE)
}

collect_ooe_protocols <- function() {
  url <- "https://www.land-oberoesterreich.gv.at/ltgspsuche.htm"
  doc <- safe_fetch_html(url)
  period_pages <- extract_links(doc, url) |>
    dplyr::filter(stringr::str_detect(tolower(paste(.data$text, .data$url)), "gesetzgebungsperiode|\\bgp\\b")) |>
    dplyr::distinct(.data$url) |>
    dplyr::pull(.data$url)

  nested <- purrr::map_dfr(period_pages, function(u) {
    d <- safe_fetch_html(u)
    extract_links(d, u)
  })

  links <- nested |>
    dplyr::filter(
      stringr::str_detect(tolower(.data$url), "internetltgbeilagenanzeige\\.jsp"),
      stringr::str_detect(tolower(.data$text), "wortprotokoll")
    )

  links_to_protocols(links, state = "ooe", source_url = url, backend = "ooe")
}

collect_sbg_protocols <- function() {
  url_new <- paste0(
    "https://service.salzburg.gv.at/lpi/searchExtern?datumVon=01.01.1994&datumBis=",
    format(Sys.Date(), "%d.%m.%Y"),
    "&artId=3&fraktionId=&periode=&session=&beilage=&titel=&text=&search="
  )
  doc_new <- safe_fetch_html(url_new)
  links_new <- extract_links(doc_new, url_new) |>
    dplyr::filter(stringr::str_detect(tolower(paste(.data$text, .data$url)), "protokoll|sitzung|stenograph|\\.pdf|\\.doc"))

  url_old <- "https://alex.onb.ac.at/slt.htm"
  doc_old <- safe_fetch_html(url_old)
  alex_index <- extract_links(doc_old, url_old) |>
    dplyr::filter(stringr::str_detect(tolower(paste(.data$text, .data$url)), "protokoll|stenograph|sitzung|slt|\\d{4}|\\.pdf")) |>
    dplyr::slice_head(n = 120)
  links_old <- alex_index |>
    dplyr::filter(stringr::str_detect(tolower(paste(.data$text, .data$url)), "protokoll|stenograph|sitzung|\\.pdf|\\.tif|\\.jpg"))

  links <- dplyr::bind_rows(links_new, links_old)
  links_to_protocols(links, state = "sbg", source_url = url_new, backend = "sbg")
}
