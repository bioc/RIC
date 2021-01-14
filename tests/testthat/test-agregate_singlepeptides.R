test_that("agregate_singlepeptides throws error without valid input", {
  data("QWCLpeptidessmall")
  data("SV_seq")
  mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
  source(mapPeptidespath)
  library(QFeatures)
  expect_error (agregate_singlepeptides("QWCLpeptidessmall",  c(1,2,4,3,6,5,7,10,9,8), hour18))
  expect_error (agregate_singlepeptides(QWCLpeptidessmall,  "1", names_samples=  paste(c( "hour18", "hour4",  "mock" ),rep(1:3,each=3),sep='_')))
}) 
  
  
test_that("row_peptides are a dataframe", {
  expect_true(is.data.frame(rowData(QWCLpeptidessmall)%>% as.data.frame() ))
})