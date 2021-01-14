
aggregatedsmall<-data.frame(ENSGid=c("ENSG00000100030", "ENSG00000100836" ,"ENSG00000104131"),symbol=c("MAPK1", "PABPN1", "EIF3J"),Know_RBP=c("no", "known_RBP", "known_RBP"),hour18_1=runif(3),hour4_1=runif(3),mock_1=runif(3),hour18_2=runif(3),hour4_2=runif(3),mock_2=runif(3),hour18_3=runif(3),hour4_3=runif(3),mock_3=runif(3))

test_that("remove_batcheffect throws error without valid input", {

  expect_error (remove_batcheffect("aggregatedsmall",batch2 = c("A", "A", "A", "B", "B", "B", "C", "C", "C"), sample_names = c("hour18", "hour4", "mock")))
  expect_error (remove_batcheffect(aggregatedsmall,A, sample_names = c("hour18", "hour4", "mock")))
  expect_error (remove_batcheffect(aggregatedsmall,batch2 = c("A", "A", "A", "B", "B", "B", "C", "C", "C"), hour ))
 
  expect_error (remove_batcheffect(aggregatedsmall,batch2 = c("A", "A", "A", "B", "B", "B", "C", "C", "C"), sample_names = c("day")))
  expect_error (remove_batcheffect(aggregatedsmall,batch2 = c("A", "A", "A", "B", "B", "C", "C"), sample_names = c("hour18", "hour4", "mock")))

  
   }) 

test_that("remove_batcheffect returns a dataframe",{

  expect_true(is.data.frame(remove_batcheffect(aggregatedsmall,batch2 = c("A", "A", "A", "B", "B", "B", "C", "C", "C"))))

})
