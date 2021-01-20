WCLpeptidesfilesmall_path<- system.file("extdata","WCL_peptides_small.txt", package = "RIC")
j <- str_which(colnames(WCLpeptides),str_c(c("Intensity.((\\D)).18_M_4","Intensity.((\\D)).4_18_M","Intensity.((\\D)).M_4_18"), collapse="|"))
QWCLpeptidessmall <- readQFeatures(WCLpeptidesfilesmall_path, ecol = j, sep = "\t", name = "peptides", fnames = "Sequence")

usethis::use_data(QWCLpeptidessmall , overwrite = TRUE)