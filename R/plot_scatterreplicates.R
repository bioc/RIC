
#aggregated_peptides<-aggregatedaggregated_peptides_batch
#protein_1 <- "SV_wt_nsP2"
#protein_2 <- "SV_wt_E2"

#' plot_scatterreplicates
#'
#' @param aggregated_peptides Dataframe
#' output from \code{\link{agregagte_singlepeptides}}
#' @param protein_1 Character,
#' Name of selected protein (1) to highlight on the plot
#' @param protein_2 Character,
#' Name of selected protein (2) to highlight on the plot
#' @param xlimits Integer,
#' Sets the x-axis limits on plot.
#' @param ylimits Integer,
#' Sets the y-axis limits on plot.
#' @param col1 Character,
#' Sets the color to highlight protein_1 on the plot
#' @param col2 Character,
#' Sets the color to highlight protein_2 on the plot
#' @param repx Character,
#' Sets the replicate to represent on the x-axis
#' @param repy Character,
#' Sets the replicate to represent on the y-axis
#' @examples
#' if(interactive()){
#' plot_scatterreplicates(aggregatedWCL_batch,
# 'protein_1 = "SV_wt_nsP2", protein_2= "SV_wt_E2",
#' xlimits=c(20,34), ylimits=c(20,34),
#' repx = "hour18_1",repy = "hour18_2")
#' }
#' @return A scatter plot 
#' @import stringr
#' @export
plot_scatterreplicates <- function (aggregated_peptides,
                                    protein_1,
                                    protein_2,
                                    xlimits, ylimits,
                                    col1="red",
                                    col2="blue", 
                                    repx = "hour18_1",
                                    repy = "hour18_2") {
  
  if(is.integer(xlimits)) xlimits <- as.numeric(xlimits)
  if(is.integer(ylimits)) ylimits <- as.numeric(ylimits)
  
  assertthat::assert_that(is.data.frame(aggregated_peptides),
                          is.character(protein_1),
                          is.character(protein_2),
                          is.numeric(xlimits),
                          length(xlimits) == 2,
                          is.numeric(xlimits),
                          length(xlimits) == 2,
                          is.character(col1),
                          length(col1) == 1,
                          is.character(col2),
                          length(col2) == 1,
                          is.character(repx),
                          length(repx) == 1,
                          is.character(repy),
                          length(repy) == 1)
  
  # show message in case this doesnt exists....
grepl(protein_1,aggregated_peptides$ENSGid) -> A
grepl(protein_2,aggregated_peptides$ENSGid) -> B

# aggregated_peptides %>% ggplot()+geom_point(aes(Intensity.L.18_M_4,Intensity.L.4_18_M ))

plot(aggregated_peptides[,str_which(repx, names(aggregated_peptides))], aggregated_peptides[,str_which(repy, names(aggregated_peptides))], col=alpha('gray',0.2),
           pch=19,xlim=xlimits, ylim=ylimits,
           xlab= str_glue("Abundance aggregated_peptides sample {repx}"), ylab=str_glue("Abundance aggregated_peptides sample {repy}" ))
title(main=str_glue("Protein Abundance in aggregated_peptides \n {repx} and {repy}") )

points(aggregated_peptides[A,4], aggregated_peptides[A,7], col=col1, pch=19)
text(aggregated_peptides[A,4], aggregated_peptides[A,7],  labels=str_remove(protein_1,"SV_wt_"), cex=1, pos=3)

points(aggregated_peptides[B,4], aggregated_peptides[B,7], col=col2, pch=19)
text(aggregated_peptides[B,4], aggregated_peptides[B,7],
     labels=str_remove(protein_2,"SV_wt_"), cex= 1, pos=3)

}
