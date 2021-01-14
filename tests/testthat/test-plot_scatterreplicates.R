test_that("plot_scatterreplicates throws error without valid input", {
  data("QWCLpeptidessmall")
  data("SV_seq")
  mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
  source(mapPeptidespath)
  library(QFeatures)
  expect_error (plot_scatterreplicates("aggregatedWCL_batch",  xlimits=c(20,34), ylimits=c(20,34), repx = "hour18_1",repy = "hour18_2"))
  expect_error (plot_scatterreplicates(aggregatedWCL_batch,  "c(20,34)", ylimits=c(20,34), repx = "hour18_1",repy = "hour18_2"))
  expect_error (plot_scatterreplicates(aggregatedWCL_batch,  xlimits=c(20,34), "c(20,34)", repx = "hour18_1",repy = "hour18_2"))
  expect_error (plot_scatterreplicates(aggregatedWCL_batch,  xlimits=c(20,34), ylimits=c(20,34), hour18_1,repy = "hour18_2"))
  expect_error (plot_scatterreplicates(aggregatedWCL_batch,  xlimits=c(20,34), ylimits=c(20,34), repx = "hour18_1",hour18_2))

  }) 


aggregatedsmall<-data.frame(ENSGid=c("ENSG00000100030", "ENSG00000100836" ,"ENSG00000104131"),symbol=c("MAPK1", "PABPN1", "EIF3J"),Know_RBP=c("no", "known_RBP", "known_RBP"),hour18_1=runif(3),hour4_1=runif(3),mock_1=runif(3),hour18_2=runif(3),hour4_2=runif(3),mock_2=runif(3),hour18_3=runif(3),hour4_3=runif(3),mock_3=runif(3))


