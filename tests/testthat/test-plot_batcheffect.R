aggregatedsmall<-data.frame(ENSGid=c("ENSG00000100030", "ENSG00000100836" ,"ENSG00000104131"),symbol=c("MAPK1", "PABPN1", "EIF3J"),Know_RBP=c("no", "known_RBP", "known_RBP"),hour18_1=runif(3),hour4_1=runif(3),mock_1=runif(3),hour18_2=runif(3),hour4_2=runif(3),mock_2=runif(3),hour18_3=runif(3),hour4_3=runif(3),mock_3=runif(3))


test_that("plot_batcheffect throws error without valid input", {
  expect_error (plot_batcheffect("aggregatedsmall"))
  expect_error (plot_batcheffect("aggregatedsmall",sample_names="year"))
  expect_equal(str_which(colnames(aggregatedsmall),c("hour18", "hour4" , "mock" )), c( 4, 5 , 6 , 7,  8,  9, 10, 11, 12))  

  columns<-str_which(colnames(aggregatedsmall),c("hour18" ,"hour4" ))
  expect_type(columns, "integer")
  expect_error (plot_batcheffect(aggregatedsmall,"year"))
  
}) 
 
