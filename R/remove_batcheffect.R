#' Perform batch effect correction
#' 
#' \code{remove_batcheffect} performs batch correction given the second serie
#' of batches and a vector indicating the sample names. 
#'
#' @param aggregated_peptides Dataframe 
#'  output from \code{\link{agregate_singlepeptides}}
#' @param batch2 Character,
#' indicates the corrected series of batches
#' @param sample_names Character(1),
#' indicates sample names
#' @examples
#' if(interactive()){
#' aggregatedWCL_batch<-remove_batcheffect(aggregatedWCL,batch2)
#' }
#' @return dataframe correted with \code{\link[limma]{removeBatchEffect}}
#' @import limma
#' @export
remove_batcheffect <- function (aggregated_peptides, 
                                batch2 = c("A","A","A","B","B","B","C","C","C"),
                                sample_names=c('hour18','hour4','mock')) {
  
  assertthat::assert_that(is.data.frame(aggregated_peptides),
                          is.character(batch2),
                          is.character(sample_names))
  
  
  columns<-grep("hour18|hour4|mock" ,colnames(aggregated_peptides))
    if(length(columns) < 1) {
      stop("specified sample_names ('", sample_names,
           "') are not in ",colnames(aggregated_peptides),
           "\nRun use the appropriate sample_names argument",
           call. = FALSE)
    }
  
  lengthcolumns<-length(colnames(aggregated_peptides))
  if(lengthcolumns-3 != length(batch2)) {
    stop("specified batch2 ('", batch2,
         "') does not have same length as aggregated_peptides ",lengthcolumns-3,
         "\nRun use an appropriate sequence for batch2",
         call. = FALSE)
  }
  
aggregated_peptides[,str_which(colnames(aggregated_peptides),sample_names)] <-
removeBatchEffect(aggregated_peptides[,str_which(colnames(aggregated_peptides),sample_names)] 
                                             , batch2) 
aggregated_peptides
}
