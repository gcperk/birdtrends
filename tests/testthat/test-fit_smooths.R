test_that("fit_smooths results correct", {
  indat3 <- as.data.frame(bbs_smooth_data)
  results = fit_smooths(indat3)
  expect_equal(ncol(results), 3)
  expect_type(results$proj_y, "double")
})

test_that("fit_smooths fails appropriately", {
  #results = fit_hgam(readRDS(test_path("testdata", "input1_test.rds")))
  expect_error(fit_smooths("a"))
})
