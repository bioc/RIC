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
test_that("plot_volcanoRIC throws error without valid input", {
  expect_error(plot_volcanoRIC("tabletoplotWCLsmall"))
  expect_error(plot_volcanoRIC(tabletoplotWCLsmall[,-6]))
  expect_error(plot_volcanoRIC(tabletoplotWCLsmall[,-4]))
  
}) 


test_that("plot_volcanoRIC returns a volcano plot with correct colors", {
  expect_true(is.ggplot(plot_volcanoRIC(tabletoplotWCLsmall)))
  expect_true(unlist(plot_volcanoRIC(tabletoplotWCLsmall))$labels.x=="log2 fold change")
  expect_true(unlist(plot_volcanoRIC(tabletoplotWCLsmall))$labels.y=="-log10 p-value")
  expect_true(unlist(plot_volcanoRIC(tabletoplotWCLsmall))$theme.axis.line.colour== "black")
  })


