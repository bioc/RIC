WCLfile<- system.file(
  "extdata",
  "WCL_proteinGroups.txt",
  package = "RIC"
)
WCL.raw <- read.delim(WCLfile,stringsAsFactors = FALSE)
ratios = "normalized."
columns <- colnames(WCL.raw)[grep(ratios, colnames(WCL.raw))]
usethis::use_data(WCL.raw, overwrite = TRUE)

RICfile<- system.file(
  "extdata",
  "RIC_proteinGroups.txt",
  package = "RIC"
)
RIC.raw <- read.delim(RICfile,stringsAsFactors = FALSE)
ratiosric = "normalized."
columns_ric <- colnames(RIC.raw)[grep(ratiosric, colnames(RIC.raw))]
usethis::use_data(RIC.raw, overwrite = TRUE)
save(RIC.raw,file="/Users/maria/Documents/RIC/data/RIC.raw.rda", 
     compress = "xz", compression_level = 9)

WCLpeptidesfile<- system.file(
  "extdata",
  "WCL_peptides.txt",
  package = "RIC"
)
WCLpeptides.raw <- read.delim(WCLpeptidesfile,stringsAsFactors = FALSE)
ratios = "normalized."
columns <- colnames(WCLpeptides.raw)[grep(ratios, colnames(WCLpeptides.raw))]
usethis::use_data(WCLpeptides.raw, overwrite = TRUE)
save(ProtFeatures,file="/Users/maria/Documents/RIC/data/ProtFeatures.rda", 
     compress = "xz", compression_level = 9)


RICpeptidesfile<- system.file(
  "extdata",
  "RIC_peptides.txt",
  package = "RIC"
)
RICpeptides.raw <- read.delim(RICpeptidesfile,stringsAsFactors = FALSE)
ratiosric = "normalized."
columns_ric <- colnames(RICpeptides.raw)[grep(ratiosric, colnames(RICpeptides.raw))]
usethis::use_data(RICpeptides.raw , overwrite = TRUE)


WCLpeptidesfilesmall_path<- system.file("extdata","WCL_peptides_small.txt", package = "RIC")
j <- str_which(colnames(WCLpeptides),str_c(c("Intensity.((\\D)).18_M_4","Intensity.((\\D)).4_18_M","Intensity.((\\D)).M_4_18"), collapse="|"))
QWCLpeptidessmall <- readQFeatures(WCLpeptidesfilesmall_path, ecol = j, sep = "\t", name = "peptides", fnames = "Sequence")

usethis::use_data(QWCLpeptidessmall , overwrite = TRUE)