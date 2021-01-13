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


aggregatedsmall<-agregagte_singlepeptides(QWCLpeptidessmall,SV_seq, whichorder = c(1,2,4,3,6,5,7,10,9,8), names_samples=  paste(c( "hour18","hour4" ,"mock"),rep(1:3,each=3),sep='_') )




test_that("plot_scatterreplicates  returns a plot ", {
  expect_identical(plot_scatterreplicates(aggregatedsmall,protein_1 = "ENSG00000100030", protein_2= "ENSG00000142453", xlimits=c(20,34), ylimits=c(20,34), repx = "hour18_1",repy = "hour18_2"),
                   plot_scatterreplicates(aggregatedsmall,protein_1 = "ENSG00000100030", protein_2= "ENSG00000142453", xlimits=c(20,34), ylimits=c(20,34), repx = "hour18_2",repy = "hour18_1"))

  
  })
