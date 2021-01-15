data("QWCLpeptidessmall")
data("SV_seq")

mapPeptidespath<- system.file( "scripts", "mapPeptides.R", package = "RIC")
source(mapPeptidespath)
library(QFeatures)
library(Biostrings)
ProtFeaturessmall<-list()
ProtFeaturessmall$ProtSeq<-AAStringSet(c("MSQQKQQSW", "DLKNVFPPEVAVFEPSEAEISHTQKAT", "PALREATAKGISFSSLPSTMESDKMLYMESPRTVDEKLKGD"))
names(ProtFeaturessmall$ProtSeq)<- c("A0A183","A0A5B9","A0AUZ9")
ProtFeaturessmall$GeneName<-array(c("ENSG00000235942",NA,"ENSG00000144445" ), c(1,3))
colnames(ProtFeaturessmall$GeneName)<-c("A0A183","A0A5B9","A0AUZ9")


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
 PeptideSet = row_peptides$Sequence, c(ProtFeaturessmall$ProtSeq, SV_seq),
 verbose = FALSE)
test_that("plot_singlepeptides generates a list of ProtIDs correctly", {
  expect_type(ProtIDs_who, "list")
})

ENSGid_who <-lapply(ProtIDs_who,function(x) {y=unname(ProtFeaturessmall$GeneName[x])
unique(y[!is.na(y)])})

test_that("plot_singlepeptides generates a list of ENSGid correctly", {
  expect_type(ENSGid_who, "list")
})



