

test_that("plot_singlepeptides throws error without valid input", {
  data("QWCLpeptidessmall")
  data("SV_seq")
  mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
  source(mapPeptidespath)
  library(QFeatures)
  expect_error( plot_singlepeptides("QWCLpeptidessmall", SV_seq, ENSG = TRUE, plot = TRUE))
  expect_error( plot_singlepeptides(QWCLpeptidessmall, "SV_seq", ENSG = TRUE, plot = TRUE))
  expect_error( plot_singlepeptides(QWCLpeptidessmall, SV_seq, ENSG = "TRUE", plot = TRUE))
  expect_error( plot_singlepeptides(QWCLpeptidessmall, SV_seq, ENSG = TRUE, plot = "TRUE"))
  
})

test_that("row_peptides are a dataframe", {
  expect_true(is.data.frame(rowData(QWCLpeptidessmall)%>% as.data.frame() ))
})

test_that("ProtIDs_who are a list", {
  expect_true(is.list(mapPeptides(PeptideSet=as.data.frame(rowData(QWCLpeptidessmall[["peptides"]]))$Sequence,
                        c(ProtFeatures$ProtSeq,SV_seq), verbose=FALSE)))

})

test_that("sequence is on the correct position", {
expect_equal(grep("Sequence",colnames(rowData(QWCLpeptidessmall) %>% as.data.frame())),3)
  
  
})
  

test_that("plot_singlepeptides  returns a ggplot object", {
  expect_true(is.ggplot(plot_singlepeptides(QWCLpeptidessmall,SV_seq)))
  expect_true(unlist(plot_singlepeptides(QWCLpeptidessmall,SV_seq))$labels.title== "Peptides mapping")
  expect_true(unlist(plot_singlepeptides(QWCLpeptidessmall,SV_seq))$labels.y== "Number of peptides")
  expect_true(unlist(plot_singlepeptides(QWCLpeptidessmall,SV_seq))$labels.fill== "Gene matches")
  
})
