data("QWCLpeptidessmall")
data("SV_seq")
mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
source(mapPeptidespath)
library(QFeatures)


test_that("agregate_singlepeptides throws error without valid input", {
  expect_error (agregate_singlepeptides("QWCLpeptidessmall",SV_seq,  whichorder = c(1,2,4,3,6,5,7,10,9,8),	 names_samples=   paste(sample_names,rep(1:3,each=3),sep='_')))
  expect_error (agregate_singlepeptides(QWCLpeptidessmall,"SV_seq",  whichorder = c(1,2,4,3,6,5,7,10,9,8),	 names_samples=   paste(sample_names,rep(1:3,each=3),sep='_')))
  expect_error (agregate_singlepeptides(QWCLpeptidessmall,SV_seq, "1",	 names_samples=   paste(sample_names,rep(1:3,each=3),sep='_')))
  expect_error (agregate_singlepeptides(QWCLpeptidessmall,SV_seq,  whichorder = c(1,2,4,3,6,5,7,10,9,8),	 names_samples=  c(1,2)))
  
  }) 
  
test_that("agregate_singlepeptides converts integers to numeric", {
  expect_true(is.numeric(as.numeric(c(1L,2L,4L,3L,6L,5L,7L,10L,9L,8L))))
  a<-as.numeric(c(1L,2L,4L))
  expect_equal(a,c(1,2,4))
})

test_that("agregate_singlepeptides stops with error if input is incorrect", {
  expect_error (agregate_singlepeptides(QWCLpeptidessmall,SV_seq,  whichorder = c(1,2,4,3,9,8),	 names_samples=   paste(sample_names,rep(1:3,each=3),sep='_')))
  expect_error (agregate_singlepeptides(QWCLpeptidessmall,SV_seq,  whichorder = c(1,2,4,3,6,5,7,10,9,8),	 names_samples=   paste(sample_names,rep(1:3,each=2),sep='_')))
  })

test_that("agregate_singlepeptides puts columns first", {
  a<-grep("sequences",assay(QWCLpeptidessmall) %>% as.data.frame() %>%rownames_to_column("sequences") %>% colnames())
  expect_equal (a,1)

})

test_that("row_peptides are a dataframe", {
  expect_true(is.data.frame(rowData(QWCLpeptidessmall)%>% as.data.frame() ))
})