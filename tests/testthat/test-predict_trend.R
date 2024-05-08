test_that("predict_trend results correct", {
  indata1 <- readRDS(test_path("testdata", "fit_hgam_model.rds"))
  tr = get_trend(readRDS(test_path("testdata", "fit_hgam_model.rds")))
  pred_tr =  predict_trend(indata1, tr, start_yr = 1978, proj_yr = 2000)
  expect_equal(ncol(pred_tr), 4)
  expect_equal(length(pred_tr$draw), 330)
})

test_that("predict_trend fails appropriately", {
  expect_error( predict_trend("aa", tr, start_yr = 1978, proj_yr = 2000))
  expect_error( predict_trend(indata1, tr, start_yr = 1999, proj_yr = 2000))
})

#testthat::test_file("tests/testthat/test-predict_trend.R")
#devtools::test_coverage()
