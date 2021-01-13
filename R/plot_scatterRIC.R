

# test_moderateRIC(aggregatedRIC)[[1]]$diff_hour18_hour4 -> tabletoplot
# test_moderateRIC(aggregatedRIC)[[2]]$diff_hour18_hour4 -> intensitiestoplot

#' Title
#'
#' @param tabletoplot Dataframe
#' First element of list output from \code{\link{test_moderateRIC}}
#' @param intensitiestoplot Matrix
#' Second element of l=list output from \code{\link{test_moderateRIC}}
#' @examples
#' if(interactive()){
#' test_moderateRIC(aggregatedWCL_batch)[[1]]$diff_hour18_hour4 -> tabletoplotWCL
#' test_moderateRIC(aggregatedWCL_batch)[[2]]$diff_hour18_hour4 ->  intensitiestoploWCL
#' plot_scatterRIC(tabletoplotWCL,intensitiestoploWCL)
#' }
#' @return A matrix of scatterplots
#' @export
 plot_scatterRIC <- function (tabletoplot,intensitiestoplot) {
   assertthat::assert_that(is.data.frame(tabletoplot),
                           is.matrix(intensitiestoplot))
   if(!(grep("p.adj",colnames(tabletoplot)))){
     stop(paste0("p.adj \n",
                 "is not in ",
                 colnames(tabletoplot) ),
          call. = FALSE)
   }
   
  col = ifelse(tabletoplot$p.adj <= 0.1, "orange", "gray")
  col[which(tabletoplot$p.adj <= 0.05)] = "firebrick1"
  col[which(tabletoplot$p.adj <= 0.01)] = "firebrick"
  col[which(tabletoplot$p.adj <= 0.1 & tabletoplot$log2FC < 0)] = "turquoise1"
  col[which(tabletoplot$p.adj <= 0.05 & tabletoplot$log2FC < 0)] = "turquoise4"
  col[which(tabletoplot$p.adj <= 0.01 & tabletoplot$log2FC < 0)] = "blue"
  pairs(intensitiestoplot,col=col, pch=19,labels = c("Replicate 1", "Replicate 2", "Replicate 3"))

 }
 
