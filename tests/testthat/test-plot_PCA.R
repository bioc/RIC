data("WCL.raw")

test_that("plot_PCA throws error without valid input", {
  expect_error(plot_PCA("WCL.raw", x = 1, y = 2, plot = TRUE, legend = "Conditions"))
  expect_error(plot_PCA(WCL.raw, x = "1", y = 2, plot = TRUE, legend = "Conditions"))
  expect_error(plot_PCA(WCL.raw, x = 1, y = "2", plot = TRUE, legend = "Conditions"))
  expect_error(plot_PCA(WCL.raw, x = 1, y = 2, plot = "TRUE", legend = "Conditions"))
  expect_error(plot_PCA(WCL.raw, x = 1, y = 2, plot = TRUE, legend = Conditions))
  
})

