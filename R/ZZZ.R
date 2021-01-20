.onLoad <- function(libname = find.package("RIC"), pkgname = "RIC"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      c( #globalVariables
        "MapProt2Ensg","assay","QWCLpeptidesfiltered_clean",
        "mapPeptides","ProtFeatures","listLen","aggregate",
        "Group.1","enigmRBP","text","colData","QWCLpeptides",
        "legend","combn","median","p.adjust","points","rowData",
        "text","title",
        "peptides", "Sequence","whichorder",
        "sample_names", "batch2",
      "x","y","protein_1","protein_2","xlimits","ylimits",
      "repx","repy","ENSGid","symbol","Know_RBP",
      "coefficients","sig","p.value","p.adj","log2FC",
      "test_moderateRIC","ENSG2category","ENSGannotation",
      "QWCLpeptidesfiltered_clean","RIC.raw","SV_seq","WCL.raw","enigmRBP"
      )
    )
  invisible()
}
