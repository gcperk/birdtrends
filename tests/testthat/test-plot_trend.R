test_that("plot trend results correct", {
  indat1 <- readRDS(test_path("testdata", "input1_test.rds"))
  ldf <-  readRDS(test_path("testdata", "fit_hgam_model.rds"))
  tr = get_trend(ldf)
  pred_sm =  proj_trend(ldf, tr, start_yr = 1978, proj_yr = 2020)
  pl <- plot_trend(indat1, ldf, pred_sm, start_yr = 1968, end_yr = 1977, ref_yr = 1977 )
  expect_silent(plot_trend(indat1, ldf, pred_sm, start_yr = 1968, end_yr = 1977, ref_yr = 1977))
  #expect_equal(ncol(tr), 3)
  #expect_equal(length(tr$draw), 10)
})

test_that("plot trendfails appropriately", {
  indat1 <- readRDS(test_path("testdata", "input1_test.rds"))
  ldf <-  readRDS(test_path("testdata", "fit_hgam_model.rds"))
  tr = get_trend(ldf)
  pred_sm =  proj_trend(ldf, tr, start_yr = 1978, proj_yr = 2020)
  expect_message(plot_trend(indat1, ldf, pred_sm, start_yr = 1900, end_yr = 1977, ref_yr = 1977))
  expect_message(plot_trend(indat1, ldf, pred_sm, start_yr = 1978, end_yr = 2020, ref_yr = 1977))
  expect_error(plot_trend("a",ldf, pred_sm, start_yr = 1900, end_yr = 1977, ref_yr = 1977))
  #expect_error(plot_trend(indat1,ldf, pred_sm, start_yr = NA, end_yr = 1977, ref_yr = 1977))
})

#testthat::test_file("tests/testthat/test-plot_trend.R")
