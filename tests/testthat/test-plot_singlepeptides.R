

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

