#' Multidimensional scaling across replicates
#'  
#' \code{plot_batcheffect} plots the level of similarity across replicates.
#' 
#' @param aggregated_peptides Dataframe
#' output from \code{\link{agregagte_singlepeptides}}
#' @param sample_names character
#' indicates sample names
#' @return Multidimensional scaling plot
#' @examples 
#' if(interactive()){
#' plot_batcheffect(aggregatedWCL)
#' }
#' @import limma
#' @import stringr
#' @export
plot_batcheffect <- function (aggregated_peptides,sample_names=c('hour18','hour4','mock')) {
  
  assertthat::assert_that(is.data.frame(aggregated_peptides),
                          is.character(sample_names))
  
  
  columns<-str_which(colnames(aggregated_peptides),sample_names)
  if(length(columns) < 1) {
    stop("specified sample_names ('", sample_names,
         "') are not in ",colnames(aggregated_peptides),
         "\nRun use the appropriate sample_names argument",
         call. = FALSE)
  }
  

MDS_who=plotMDS(aggregated_peptides[,str_which(colnames(aggregated_peptides),sample_names)],
                labels = NULL, pch= c(19), col= c(3:1), cex = 1,
                gene.selection = "pairwise", main='Multidimensional scaling Input',
                xlab = "Leading logFC dimension 1", 
                ylab = "Leading logFC dimension 2")
text(x=MDS_who$x, y = MDS_who$y, labels=colData(QWCLpeptides)$sample, pos= 4, col=c(3:1))
legend("topright", legend=colData(QWCLpeptides)$group[1:3], col=3:1, pch=19)

}


