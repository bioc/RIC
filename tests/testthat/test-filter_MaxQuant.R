data("WCL.raw")

test_that("filter_MaxQuant throws error without valid input", {
  expect_error(filter_MaxQuant("WCL.raw", tofilter = c("Reverse", "Potential.contaminant"),
                               ratios = "Ratio.H.L.normalized"))
 })

test_that("filter_MaxQuant throws error without valid input", {
  expect_error(filter_MaxQuant(WCL.raw, tofilter = c("Reverse", "Potential.contaminant"),
                               ratios = Ratio.H.L.normalized))
})


