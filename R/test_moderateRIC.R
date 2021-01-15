#' test_moderateRIC
#'
#' @param aggregated_peptides Dataframe
#'  output from \code{\link{agregagte_singlepeptides}}
#' @return Invisibly return a list with diff_table and diff_intensities for each timepoint
#' @examples
#' if(interactive()){
#' test_moderateRIC(aggregatedWCL_batch)[[1]]$diff_hour18_hour4 -> tabletoplotWCL
#' test_moderateRIC(aggregatedWCL_batch)[[2]]$diff_hour18_hour4 ->  intensitiestoploWCL
#' }
#' @import limma
#' @export
test_moderateRIC <- function (aggregated_peptides,sample_names=c("hour18", "hour4",  "mock" )) {
  
  assertthat::assert_that(is.data.frame(aggregated_peptides))

  if(!any(grepl("hour",colnames(aggregated_peptides)) | grepl("mock",colnames(aggregated_peptides)))){
    stop(paste0("hour or mock \n",
                "are not contained in ",
                names(aggregated_peptides) ),
         call. = FALSE)
  }
  
Intensities = as.matrix(aggregated_peptides[,grepl("hour",colnames(aggregated_peptides)) | grepl("mock",colnames(aggregated_peptides))])
cond = sapply(strsplit(colnames(Intensities), split="_"),
              function(x) { x[1] })
sample_combi=combn(sample_names,2,simplify=TRUE)
colnames(sample_combi) =
  apply(sample_combi,2,
        function(x) {
          paste('diff',paste(x,collapse='_'),sep='_')
        })

diff_table = diff_intensities = list()
for (s in colnames(sample_combi)) {
  sample1 = Intensities[,cond == sample_combi[1,s]]
  sample2 = Intensities[,cond == sample_combi[2,s]]
  X =sample1-sample2
  # This segment performs a median correction
  for (j in seq_along(sample_names)) {
    X[,j] = X[,j] - median(X[,j],na.rm = TRUE)}
  diff_intensities[[s]] = X
  fit=eBayes(lmFit(X))
  fit$p.adj=p.adjust(fit$p.value,method="BH")
  fit$p.adj[is.na(fit$p.adj)]=1
  dt = data.frame(ENSGid = aggregated_peptides$ENSGid,
                  symbol = aggregated_peptides$symbol,
                  Know_RBP = aggregated_peptides$Know_RBP,
                  log2FC = fit$coefficients[,1],
                  p.value = fit$p.value[,1],
                  p.adj = fit$p.adj,
                  stringsAsFactors=FALSE)
  dt$sig=''
  dt$sig[dt$p.adj<0.1]='*'
  dt$sig[dt$p.adj<0.01]='**'
  dt=dt[,c(1,2,3,4,5,6,7)]
  diff_table[[s]] = dt
  
}
invisible(list(diff_table,diff_intensities))
}



        