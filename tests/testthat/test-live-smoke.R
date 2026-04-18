test_that("live smoke test for selected states", {
  skip_if_offline()
  skip_on_cran()

  targets <- c("bgld", "stm", "vbg")
  res <- purrr::map(targets, ~try(list_protocols(.x, limit = 3), silent = TRUE))

  ok <- purrr::map_lgl(res, ~!inherits(.x, "try-error") && nrow(.x) >= 1)
  expect_true(any(ok))
})
