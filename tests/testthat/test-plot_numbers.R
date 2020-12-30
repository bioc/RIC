data("WCL.raw")
test_that("plot_numbers throws error without valid input", {
  expect_error(plot_numbers("WCL.raw"))
})
