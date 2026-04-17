test_that("catalog is complete", {
  s <- list_states()
  expect_equal(nrow(s), 9)
  expect_true(all(c("state", "state_name", "entry_url") %in% names(s)))
})

test_that("feature matrix is exposed", {
  f <- landtage_supported_features()
  expect_equal(nrow(f), 9)
  expect_true(all(c("sessions", "protocols", "protocol_text", "members") %in% names(f)))
})

test_that("members placeholder is graceful", {
  m <- list_members("vbg")
  expect_equal(m$backend[[1]], "not_yet_implemented")
  expect_true(is.na(m$member_name[[1]]))
})

test_that("download_protocols validates required columns", {
  expect_error(download_protocols(tibble::tibble(url = "https://example.org")))
})


test_that("protocol output contains legislative period column", {
  links <- tibble::tibble(text = "xxiii-gp protokoll", href = "/p.pdf", url = "https://example.org/p.pdf")
  x <- landtageAT:::links_to_protocols(links, state = "bgld", source_url = "https://example.org")
  expect_true("legislative_period" %in% names(x))
  expect_equal(x$legislative_period[[1]], "XXIII")
})
