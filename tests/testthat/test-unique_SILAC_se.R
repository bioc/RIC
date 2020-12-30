data("WCL.raw")

test_that("make_unique throws error without valid input", {
  expect_error(make_unique("WCL.raw", "Gene.names", "Protein.IDs"))
  expect_error(make_unique(WCL.raw, "Gene.name", "Protein.IDs"))
  expect_error(make_unique(WCL.raw, "Gene.names", "Protein.ID"))

 # expect error if neither names are valid
})

test_that("make_unique returns a data.frame", {
  expect_s3_class(make_unique(WCL.raw, "Gene.names", "Protein.IDs"), "data.frame")
  expect_s3_class(make_unique(tibble::as_tibble(WCL.raw), "Gene.names", "Protein.IDs"), "data.frame")
})

test_that("make_unique returns unique names", {
  expect_false(any(duplicated(make_unique(WCL.raw, "Gene.names", "Protein.IDs")$name)))
})


test_that("make_se throws error without valid input", {
  expect_error(make_se("WCL.raw", 90:110))
  expect_error(make_se(WCL.raw, "90:110"))

})
