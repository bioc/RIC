#' plot_PCA
#'
#' @param data_filt SE object
#' @param x numberic for PCA x axis
#' @param y numberic for PCA y axis
#' @param plot logical
#' @param legend character name for legend
#' @return a pca plot
#' @examples
#' if(interactive()){
#' plot_PCA(data_filt, x = 1, y = 2, plot = TRUE, legend = "Conditions")
#' }
#' @export
plot_PCA <- function(data_filt, x = 1, y = 2, plot = TRUE, legend="Conditions"){
  if(is.integer(x)) x <- as.numeric(x)
  if(is.integer(y)) y <- as.numeric(y)
  assertthat::assert_that(inherits(data_filt, "SummarizedExperiment"),
                          is.numeric(x),
                          length(x) == 1,
                          is.numeric(y),
                          length(y) == 1)
  
  
  pca <- prcomp(na.omit(assay(data_filt)),scale=TRUE)
  values <- pca$rotation
  
  ggplot(as.data.frame(values),aes(get(paste0("PC", x)), get(paste0("PC", y)), colour=rownames(values))) +
    geom_point(size = 3)+
    labs(x=paste0("PC", x, ": ",round(summary(pca)$importance[2,x]*100,2), "%"),
         y = paste0("PC", y, ": ",round(summary(pca)$importance[2,y]*100,2), "%"),
         colour=legend)
  
}

