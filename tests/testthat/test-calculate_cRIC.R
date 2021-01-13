aggregatedsmall<-data.frame(ENSGid=c("ENSG00000100030", "ENSG00000100836" ,"ENSG00000104131"),symbol=c("MAPK1", "PABPN1", "EIF3J"),Know_RBP=c("no", "known_RBP", "known_RBP"),hour18=runif(3),hour4=runif(3),mock=runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3))  

test_that("calculate_cRIC throws error without valid input", {
  data("QWCLpeptidessmall")
  data("SV_seq")
  mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
  source(mapPeptidespath)
  library(QFeatures)
  expect_error (calculate_cRIC("aggregatedsmall",aggregatedsmall))
  expect_error (calculate_cRIC(aggregatedsmall,"aggregatedsmall"))
  
  tomerge_by<-c('ENSGid', 'symbol','Know_RBP')
  expect_equal(grep("ENSGid", colnames(aggregatedsmall)), 1)
  expect_equal(grep("symbol", colnames(aggregatedsmall)), 2)
  expect_equal(grep("Know_RBP", colnames(aggregatedsmall)), 3)  
  expect_identical(c('ENSGid', 'symbol','Know_RBP'), tomerge_by)  

 expect_error(calculate_cRIC(aggregatedsmall[-1],aggregatedsmall))
 expect_error(calculate_cRIC(aggregatedsmall,aggregatedsmall[-1]))
 
 expect_error(calculate_cRIC(aggregatedsmall[-1],aggregatedsmall))
 
}) 

test_that("calculate_cRIC returns a dataframe object", {
  expect_true(is.data.frame(calculate_cRIC(aggregatedsmall,aggregatedsmall)))
})
