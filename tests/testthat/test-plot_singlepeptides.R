data("QWCLpeptidessmall")
data("SV_seq")

mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
source(mapPeptidespath)
library(QFeatures)
library(Biostrings)
ProtFeatures<-list()
ProtFeatures$ProtSeq<-AAStringSet(c("MSQQKQQSW", "DLKNVFPPEVAVFEPSEAEISHTQKAT", "PALREATAKGISFSSLPSTMESDKMLYMESPRTVDEKLKGD"))
names(ProtFeatures$ProtSeq)<- c("A0A183","A0A5B9","A0AUZ9")


test_that("plot_singlepeptides throws error without valid input", {
  expect_error( plot_singlepeptides("QWCLpeptidessmall", SV_seq, ENSG = TRUE, plot = TRUE))
  expect_error( plot_singlepeptides(QWCLpeptidessmall, "SV_seq", ENSG = TRUE, plot = TRUE))
  expect_error( plot_singlepeptides(QWCLpeptidessmall, SV_seq, ENSG = "TRUE", plot = TRUE))
  expect_error( plot_singlepeptides(QWCLpeptidessmall, SV_seq, ENSG = TRUE, plot = "TRUE"))
})

row_peptides <- rowData(QWCLpeptidessmall[["peptides"]]) %>% as.data.frame()  

test_that("plot_singlepeptides maps peptides to genes correctly", {
  expect_type(row_peptides, "list")
})

ProtIDs_who <- mapPeptides(
 PeptideSet = row_peptides$Sequence, c(ProtFeatures$ProtSeq, SV_seq),
 verbose = FALSE)
test_that("plot_singlepeptides generates a list of ProtIDs correctly", {
  expect_type(ProtIDs_who, "list")
})

ENSGid_who <-lapply(ProtIDs_who,function(x) {y=unname(ProtFeatures$GeneName[x])
unique(y[!is.na(y)])})

test_that("plot_singlepeptides generates a list of ENSGid correctly", {
  expect_type(ENSGid_who, "list")
})



