test_that("fit_trend results correct", {
  tr = get_trend(readRDS(test_path("testdata", "fit_hgam_model.rds")))
  expect_equal(ncol(tr), 3)
  expect_equal(length(tr$draw), 10)
})

test_that("fit_trend fails appropriately", {
  trr = readRDS(test_path("testdata", "fit_hgam_model.rds"))
  expect_error(get_trend("a"))
  expect_error(get_trend(trr, method = "NA"))
 })

#testthat::test_file("tests/testthat/test-get_trend.R")
