
test_that("get_targets fails appropriately", {
  ldf <-  readRDS(test_path("testdata", "fit_hgam_model.rds"))
  targs <- get_targets(ldf,  ref_year = 1970,  st_year = 1977, st_lu_target_pc = -2, st_up_target_pc = 1)
  expect_silent(get_targets(ldf,  ref_year = 1970,  st_year = 1977, st_lu_target_pc = -2, st_up_target_pc = 1))
  expect_equal(targs$lt_lu_target, NA_real_)
  expect_type(targs$st_lu_target, "double")
})

test_that("get_targets errors appropriate", {
  ldf <-  readRDS(test_path("testdata", "fit_hgam_model.rds"))
  expect_error(get_targets(ldf,  ref_year = 2014,  st_year = 2026, st_lu_target_pc = -2, st_up_target_pc = 1))
  expect_error(get_targets(ldf,  ref_year = 1970,  st_year = 1977, st_lu_target_pc = 5, st_up_target_pc = 1))
  #expect_error(get_targets("aa",  ref_year = 1970,  st_year = 1977, st_lu_target_pc = -2, st_up_target_pc = 1))
})

#testthat::test_file("tests/testthat/test-get_targets.R")
