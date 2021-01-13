aggregatedsmall<-data.frame(ENSGid=c("ENSG00000100030", "ENSG00000100836" ,"ENSG00000104131"),symbol=c("MAPK1", "PABPN1", "EIF3J"),Know_RBP=c("no", "known_RBP", "known_RBP"),hour18_1=runif(3),hour4_1=runif(3),mock_1=runif(3),hour18_2=runif(3),hour4_2=runif(3),mock_2=runif(3),hour18_3=runif(3),hour4_3=runif(3),mock_3=runif(3))

test_that("semiq_cRIC throws error without valid input", {
  expect_error(semiq_cRIC("aggregatedRIC",condition = "hour4",relative = "mock" ,updown=2 ))
  expect_error(semiq_cRIC(aggregatedRIC,condition = hour4,relative = "mock" ,updown=2 ))
  expect_error(semiq_cRIC(aggregatedRIC,condition = "hour4",relative = mock ,updown=2 ))
  expect_error(semiq_cRIC(aggregatedRIC,condition = "hour4",relative = "mock" ,updown="2" ))

}) 

Intensities = as.matrix(aggregatedsmall[,grepl("hour",colnames(aggregatedsmall)) | grepl("mock",colnames(aggregatedsmall))])
cond = sapply(strsplit(colnames(Intensities), split="_"),   function(x) { x[1] })
semi_condition_relative=apply(is.na(Intensities[,cond == "mock"]) - is.na(Intensities[,cond == "hour4"]), 1,  function(x) { sum(x) } )
selection = aggregatedsmall[semi_condition_relative>=2,]
selection = selection[,c(1,2,3,6,9,12,5,8,11,4,7,10)]


test_that("semiq_cRIC selects the corrects intensity columns", {
  expect_equal(as.matrix(aggregatedsmall[,-c(1:3)]),Intensities)
  expect_length(grep("_",sapply(strsplit(colnames(Intensities), split="_"),      function(x) { x[1] })),0)
  expect_type(apply(is.na(Intensities[,cond == "mock"]) - is.na(Intensities[,cond == "hour4"]), 1,  function(x) { sum(x) } ),"integer")
  expect_equal(semiq_cRIC(aggregatedsmall,condition = "hour4",relative = "mock" ,updown=2 ),
               semiq_cRIC(aggregatedsmall,condition = "hour4",relative = "mock" ,updown=3 ))
  expect_equal(semiq_cRIC(aggregatedsmall,condition = "hour4",relative = "mock" ,updown=-2 ),
               semiq_cRIC(aggregatedsmall,condition = "hour4",relative = "mock" ,updown=-3 ))
  expect_s3_class(aggregatedsmall[semi_condition_relative>=2,],"data.frame")
  expect_s3_class(aggregatedsmall[semi_condition_relative<=-2,],"data.frame")
  expect_named(aggregatedsmall[semi_condition_relative>=2,][,c(1,2,3,6,9,12,5,8,11,4,7,10)],c("ENSGid" ,  "symbol" ,  "Know_RBP" ,"mock_1",   "mock_2"  , "mock_3" ,  "hour4_1" , "hour4_2" , "hour4_3" , "hour18_1" ,"hour18_2" ,"hour18_3"))
  })

add_summary <- function(proteinset){
  hour18_index=grep('18',colnames(proteinset))
  hour4_index=grep('4',colnames(proteinset))
  mock_index=grep('mock',colnames(proteinset))
  proteinset=cbind(proteinset,    hour18_total=apply(proteinset,1,
                                      function(x) { sum(!is.na(x[hour18_index])) }))
  proteinset=cbind(proteinset,   hour4_total=apply(proteinset,1,
                                     function(x) {     sum(!is.na(x[hour4_index]))
                                     }))
  proteinset=cbind(proteinset,       mock_total=apply(proteinset,1,
                                    function(x) {  sum(!is.na(x[mock_index]))   }))
  proteinset
}

test_that("semiq_cRIC returns a dataframe with correct number of columns", {
  expect_named(add_summary(selection),c("ENSGid" ,  "symbol" ,  "Know_RBP" ,"mock_1",   "mock_2"  , "mock_3" ,  "hour4_1" , "hour4_2" , "hour4_3" , "hour18_1" ,"hour18_2" ,"hour18_3","hour18_total" ,"hour4_total",  "mock_total" ))
  expect_s3_class(semiq_cRIC(aggregatedsmall,condition = "hour4",relative = "mock" ,updown=2 ),"data.frame")
  
  })



