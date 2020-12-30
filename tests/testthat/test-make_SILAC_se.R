data("WCL.raw")
test_that("make_SILAC_se_parse throws error without valid input", {
  expect_error(make_SILAC_se_parse("WCL.raw", conditions = 3,chars = 13,length_labeling = 7))
  expect_error(make_SILAC_se_parse(WCL.raw, conditions = "3",chars = 13,length_labeling = 7))
  expect_error(make_SILAC_se_parse(WCL.raw, conditions = 3,chars = "13",length_labeling = 7))
  expect_error(make_SILAC_se_parse(WCL.raw, conditions = 3,chars = 13,length_labeling = "7"))
  expect_error(make_SILAC_se_parse(WCL.raw, conditions = 3,chars = 13,length_labeling = "7"))
})
