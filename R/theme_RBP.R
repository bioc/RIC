#' RIC ggplot theme 1
#'
#' \code{theme_RBP} is the default ggplot theme used for plotting.
#' @return ggplot theme
#' @examples
#' if(interactive()){
#' # Load example
#' data <- doxTreatment
#' data <- data[data$Reverse != "+" & data$Potential.contaminant != "+" &
#' data$Reverse != "+" ,]
#' data_unique <- make_unique(data, "Gene.names", "Protein.IDs", delim = ";")
#'
#' # Make SummarizedExperiment
#' ratios<-"ratios"
#' columns_positions<-grep(ratios, colnames(proteins_unique))
#' se <- make_SILAC_se_parse(data_unique, columns_positions,
#' invert= "ligth", numerator= "RBP",denominator= "Organic", conditions=2,
#' bioreplicates=3,chars = 0)
#' plot_numbers(se) #uses theme_RBP
#' }
#' @import ggplot2
#' @export
theme_RBP <- function() {
  # Use theme_bw() as default
  basesize <- 12
  basefamily <- "Helvetica"
  theme <- ggplot2::theme_bw(base_size = basesize, base_family=basefamily)
  scale <- ggplot2::scale_fill_brewer(palette = "Dark2")
  # Change plot title appearance
  theme$plot.title$face <- "bold"
  theme$plot.title$size <- basesize + 2
  theme$plot.title.position <- "plot"
  
  # Change axis title appearance
  theme$axis.title.x$size <- basesize + 2
  
  theme$axis.title.y$size <- basesize + 2
  
  # Change axis text appearance
  theme$axis.text$size <- basesize
  theme$axis.text$colour <- "black"
  
  # Change legend title appearance
  theme$legend.title$size <- basesize + 2
  
  # Change legend text appearance
  theme$legend.text$size <- basesize
  
  # Change strip text (facet headers) appearance
  theme$strip.text$face <- "bold"
  theme$strip.text$size <- basesize + 2
  theme$strip.text$colour <- "black"
  
  theme$axis.text.x$angle <- 90
  theme$axis.text.x$hjust <- 1
  theme$axis.text.x$vjust <- 0.5
  
  #More aesthetic
  theme$plot.title.position = 'plot'
  return(theme)
  return(scale)
}

#' RBP ggplot theme 2
#'
#' \code{theme_RBP2} is the ggplot theme used for plotting
#' in \code{\link{sequence_coverage}} change axis labels and legend
#' @return ggplot theme
#' @examples
#' if(interactive()){
#' # Load example
#' data <- doxTreatment
#' data <- data[data$Reverse != "+" & data$Potential.contaminant != "+" &
#' data$Reverse != "+" ,]
#' data_unique <- make_unique(data, "Gene.names", "Protein.IDs", delim = ";")
#'
#' # Make SummarizedExperiment
#' ratios<-"ratios"
#' columns_positions<-grep(ratios, colnames(proteins_unique))
#' se <- make_SILAC_se_parse(data_unique, columns_positions,
#' invert= "ligth", numerator= "RBP",denominator= "Organic", conditions=2,
#' bioreplicates=3,chars = 0)
#' sequence_coverage(se,
#'  number=35,label1="heavy",label2="light",
#'  phase1="Organic", phase2="RBP") #uses theme_RBP2
#' }
#' @export
theme_RBP2 <- function() {
  
  # Use theme_bw() as default
  basesize <- 12
  basefamily <- "Helvetica"
  theme <- ggplot2::theme_bw(base_size = basesize, base_family=basefamily)
  scale <- ggplot2::scale_fill_brewer(palette = "Dark2")
  
  # Change plot title appearance
  theme$plot.title$face <- "bold"
  theme$plot.title$size <- basesize + 2
  theme$plot.title.position <- "plot"
  
  # Change axis title appearance
  theme$axis.title.x$size <- basesize + 2
  
  theme$axis.title.y$size <- basesize + 2
  
  # Change axis text appearance
  theme$axis.text$size <- basesize
  theme$axis.text$colour <- "black"
  
  # Change legend title appearance
  theme$legend.title$size <- basesize + 2
  
  # Change legend text appearance
  theme$legend.text$size <- basesize
  
  # Change strip text (facet headers) appearance
  theme$strip.text$face <- "bold"
  theme$strip.text$size <- basesize + 2
  theme$strip.text$colour <- "black"
  return(theme)
  
}

#' RBP scale
#'
#' \code{scale_RBP} is the ggplot scale
#' in \code{\link{sequence_coverage}} to change colours
#' @return ggplot scale
#' @examples
#' if(interactive()){
#'
#' }
#' @export
scale_RBP<- function() {
  scale <- ggplot2::scale_fill_brewer(palette = "Dark2")
  return(scale)
  
}


#' RBP scale 2
#'
#' \code{scale_RBP} is the ggplot scale
#' in \code{\link{sequence_coverage}} to change colours
#' @return ggplot scale
#' @examples
#' if(interactive()){
#'
#' }
#' @export
scale_RBP2<- function() {
  scale <- ggplot2::scale_colour_manual(values=c("#666666", "#A6761D"))
  return(scale)
}
