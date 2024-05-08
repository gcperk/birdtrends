test_that("get_trend fails appropriately", {
  preds_sm = readRDS(test_path("testdata", "pred_tr_test.rds"))
  cp = calculate_probs(preds_sm, ref_year = 1977, targ_year = 2011, prob_decrease = NULL,prob_increase = 5)
  expect_silent(calculate_probs(preds_sm, ref_year = 1977, targ_year = 2011, prob_decrease = NULL,prob_increase = 5))
  expect_equal(ncol(cp), 3)
  expect_equal(cp$prob_increase_5_percent, 100)
})

test_that("get_trend warnings appropriate", {
  preds_sm = readRDS(test_path("testdata", "pred_tr_test.rds"))
  expect_error(calculate_probs(preds_sm, ref_year = 1900, targ_year = 2011, prob_decrease = NULL, prob_increase = 5))
  expect_error(calculate_probs(preds_sm, ref_year = 1977, targ_year = 2040, prob_decrease = NULL, prob_increase = 5))
  expect_error(calculate_probs(preds_sm, ref_year = 1977, targ_year = 2011, prob_decrease = NULL, prob_increase = "a"))
  #expect_equal(ncol(cp), 3)
  #expect_equal(cp$prob_increase_5_percent, 100)
})

#testthat::test_file("tests/testthat/test-get_trend.R")
