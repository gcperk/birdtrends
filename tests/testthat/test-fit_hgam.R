
test_that("fit_hgam results correct", {
  #indata1 <- readRDS(test_path("testdata", "input1_test.rds"))
  results = fit_hgam(readRDS(test_path("testdata", "input1_test.rds")))
  expect_equal(ncol(results), 3)
  expect_type(results$proj_y, "double")
})

test_that("fit_hgam fails appropriately", {
  #results = fit_hgam(readRDS(test_path("testdata", "input1_test.rds")))
  expect_error(fit_hgam("a"))
})
