aggregatedsmall<-data.frame(ENSGid=c("ENSG00000100030", "ENSG00000100836" ,"ENSG00000104131"),symbol=c("MAPK1", "PABPN1", "EIF3J"),Know_RBP=c("no", "known_RBP", "known_RBP"),hour18_1=runif(3),hour4_1=runif(3),mock_1=runif(3),hour18_2=runif(3),hour4_2=runif(3),mock_2=runif(3),hour18_3=runif(3),hour4_3=runif(3),mock_3=runif(3))

test_that("test_moderateRIC throws error without valid input", {
  data("QWCLpeptidessmall")
  data("SV_seq")
  mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
  source(mapPeptidespath)
  library(QFeatures)
  expect_error (test_moderateRIC("aggregatedsmall"))

  expect_error(test_moderateRIC(colnames(aggregatedsmall[1:3])))
}) 
  
Intensities = as.matrix(aggregatedsmall[,grepl("hour",colnames(aggregatedsmall)) | grepl("mock",colnames(aggregatedsmall))])
test_that("test_moderateRIC selects the corrects intensity columns", {
  expect_equal(as.matrix(aggregatedsmall[,-c(1:3)]),Intensities)
  expect_length(grep("_",sapply(strsplit(colnames(Intensities), split="_"),      function(x) { x[1] })),0)
  expect_type(combn(c( "hour18", "hour4",  "mock"  ),2,simplify=TRUE),"character")
  expect_length(grep("diff",apply(combn(c( "hour18", "hour4",  "mock"  ),2,simplify=TRUE),2, function(x) { paste('diff',paste(x,collapse='_'),sep='_')  })),3)
  }) 


  test_that("test_moderateRIC returns invisible lists", {
    expect_invisible(test_moderateRIC(aggregatedsmall))
    
  }) 
