
aggregatedsmall<-data.frame(ENSGid=c("ENSG00000100030", "ENSG00000100836" ,"ENSG00000104131"),symbol=c("MAPK1", "PABPN1", "EIF3J"),Know_RBP=c("no", "known_RBP", "known_RBP"),hour18=runif(3),hour4=runif(3),mock=runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3),runif(3))  

test_that("remove_batcheffect throws error without valid input", {
  data("QWCLpeptidessmall")
  data("SV_seq")
  mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
  source(mapPeptidespath)
  library(QFeatures)
  expect_error (remove_batcheffect("aggregatedsmall",batch2 = c("A", "A", "A", "B", "B", "B", "C", "C", "C"), sample_names = c("hour18", "hour4", "mock")))
  expect_error (remove_batcheffect(aggregatedsmall,A, sample_names = c("hour18", "hour4", "mock")))
  expect_error (remove_batcheffect(aggregatedsmall,batch2 = c("A", "A", "A", "B", "B", "B", "C", "C", "C"), hour ))
  expect_error (remove_batcheffect(aggregatedsmall,batch2 = c("A", "A", "A", "B", "B", "B", "C", "C", "C"), sample_names = c("day")))
  }) 


test_that("remove_batcheffect returns a dataframe",{
  expect_true(any(str_detect(colnames(aggregatedsmall),c( "hour18","hour4" ,"mock"))))  
  expect_true(any(str_detect(colnames(aggregatedsmall),"ENSGid")))  

})
