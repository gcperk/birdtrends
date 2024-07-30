test_that("fit_gams results correct", {
  indat2 <- as.data.frame(posterior_draws_data)[1:100,]
  results = fit_gam(indat2)
  expect_equal(ncol(results), 3)
  expect_type(results$proj_y, "double")
})

test_that("fit_smooths fails appropriately", {
  expect_error(fit_gam("a"))
  expect_error(fit_gam(as.data.frame(annual_indicies_data)))
})
