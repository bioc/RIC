tabletoplotWCLsmall <-
  data.frame(
    ENSGid = c("ENSG00000100030", "ENSG00000100836" , "ENSG00000104131"),
    symbol = c("MAPK1", "PABPN1", "EIF3J"),
    Know_RBP = c("no", "known_RBP", "known_RBP"),
    log2FC = runif(3),
    p.value = runif(3),
    p.adj = runif(3),
    sig = runif(3)
  )
intensitiestoploWCLsmall <-
  matrix(c(runif(3), runif(3), runif(3)), nrow = 3)
test_that("plot_scatterRIC throws error without valid input", {
  expect_error (plot_scatterRIC("tabletoplotWCLsmall", intensitiestoploWCLsmall))
  expect_error (plot_scatterRIC(tabletoplotWCLsmall, "intensitiestoploWCLsmall"))
  
  expect_error (plot_scatterRIC(tabletoplotWCLsmall[, -6], intensitiestoploWCLsmall))
  
})

test_that("plot_scatterRIC assigns colors based on p.adj values", {
  col <- ifelse(tabletoplotWCLsmall$p.adj <= 0.1, "orange", "gray")
  expect_type(col, "character")
  expect_type(col[which(tabletoplotWCLsmall$p.adj <= 0.05)], "character")
  expect_type(col[which(tabletoplotWCLsmall$p.adj <= 0.01)], "character")
  expect_type(col[which(tabletoplotWCLsmall$p.adj <= 0.1 &
                          tabletoplotWCLsmall$log2FC < 0)] , "character")
  expect_type(col[which(tabletoplotWCLsmall$p.adj <= 0.1 &
                          tabletoplotWCLsmall$log2FC < 0)] , "character")
})

test_that("plot_scatterRIC returns a plot", {
  expect_null(plot_scatterRIC(tabletoplotWCLsmall, intensitiestoploWCLsmall))
}) 