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

#note that in the experiment it says that fir WCL: 
#exp1 M_4_18, #exp2 18_M_4 #exp3 4_18_M