test_that("get_trend fails appropriately", {
  preds_sm = readRDS(test_path("testdata", "pred_tr_test.rds"))
  cp = trend_change(preds_sm, ref_year = 1977, targ_year = 2011)
  expect_silent(trend_change(preds_sm, ref_year = 1977, targ_year = 2011))
  expect_equal(ncol(cp), 5)
  #expect_equal(cp$prob_increase_5_percent, 100)
})

test_that("get_trend warnings appropriate", {
  preds_sm = readRDS(test_path("testdata", "pred_tr_test.rds"))
  expect_error(trend_change(preds_sm, ref_year = 1900, targ_year = 2011))
  expect_error(trend_change(preds_sm, ref_year = 1977, targ_year = 1976))
  expect_error(trend_change(preds_sm, ref_year = 1977, targ_year = "a"))
  #expect_equal(ncol(cp), 3)
  #expect_equal(cp$prob_increase_5_percent, 100)
})

#testthat::test_file("tests/testthat/test-get_trend.R")
