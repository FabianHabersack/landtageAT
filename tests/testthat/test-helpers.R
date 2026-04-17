test_that("state normalization works", {
  expect_equal(landtageAT:::normalize_state("Wien"), "wie")
  expect_equal(landtageAT:::normalize_state("kaernten"), "ktn")
  expect_error(landtageAT:::normalize_state("foo"))
})

test_that("infer_date handles multiple formats", {
  x <- c("Sitzung 13.03.2024", "2025-01-31 Plenarprotokoll", "no date")
  out <- landtageAT:::infer_date(x)
  expect_equal(as.character(out[1]), "2024-03-13")
  expect_equal(as.character(out[2]), "2025-01-31")
  expect_true(is.na(out[3]))
})

test_that("link extraction handles relative URLs", {
  html <- xml2::read_html('<html><body><a href="/docs/p1.pdf"> Protocol 01.02.2023 </a></body></html>')
  links <- landtageAT:::extract_links(html, "https://example.org/base")
  expect_equal(links$url[[1]], "https://example.org/docs/p1.pdf")
  tbl <- landtageAT:::links_to_protocols(links, state = "wie", source_url = "https://example.org")
  expect_equal(tbl$document_type[[1]], "pdf")
})

test_that("filtering keeps relevant links and removes excluded", {
  links <- tibble::tibble(
    text = c("Stenographisches Protokoll", "Impressum"),
    href = c("/p.pdf", "/impressum"),
    url = c("https://x.at/p.pdf", "https://x.at/impressum")
  )
  out <- landtageAT:::filter_links_for_state(links, "protok|pdf", "impressum")
  expect_equal(nrow(out), 1)
  expect_equal(out$url[[1]], "https://x.at/p.pdf")
})

test_that("infer_legislative_period parses common forms", {
  x <- c("xxiii-gp-protokolle", "19. Gesetzgebungsperiode", "/sitzungen/XVIII", "XXX. Landtagsperiode")
  out <- landtageAT:::infer_legislative_period(x)
  expect_equal(out[[1]], "XXIII")
  expect_equal(out[[2]], "19")
  expect_equal(out[[3]], "XVIII")
  expect_equal(out[[4]], "XXX")
})
