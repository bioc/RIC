#' make_SILAC_se_parse
#' Data.frame to SummarizedExperiment object
#' conversion using file information and user input
#' @param proteins_unique Data.frame,
#' Protein table with unique names annotated in the 'name' column
#' (output from \code{\link{make_unique}()}).
#' @param invert character,
#' SILAC channel to invert
#' @param ratios Character,
#' columns from proteinGroups.txt file that contain the data of interest.
#' @param numerator character,
#' condition of interest (RBP)
#' @param denominator character,
#' condition to make relative (Organic) (control)
#' @param columns_positions, integer
#' positions in the proteinGroups thaat contains Ration H/L normalised
#' @param chars integer,
#' number of letters to remove from end of names to make se
#' @param length_labeling integer,
#' number of letters to remove from beginning of names to make se
#' @param sep Character(1),
#' The separator used to parse the column header
#' @param conditions integer,
#' number of SILAC labels in the experiment
#' @param bioreplicates integer,
#' number of biologigical replicates
#' @return A SummarizedExperiment object
#' with log2-transformed values, normalised to control and median substracted
#' (by column) .
#' @examples
#' if(interactive()){
#' # Load example
#' data <- doxTreatment
#' data <- data[data$Reverse != "+" & data$Potential.contaminant != "+" &
#' data$Reverse != "+" ,]
#' data_unique <- make_unique(data, "Gene.names", "Protein.IDs", delim = ";")
#'
#' # Make SummarizedExperiment
#' columns_positions<-str_which(colnames(cRIC_merged),"normalized\\.[A|B|C|18|4|M]")
#' ratios <- colnames(cRIC_merged)[str_which(colnames(cRIC_merged),"normalized\\.[A|B|C|18|4|M]")]
#' se <- make_SILAC_se_parse(cRIC_merged, columns_positions,
#' invert= c("Ratio.M.L.normalized.18_M_4","Ratio.H.L.normalized.18_M_4"), 
#' numerator= c("A", "B","C"),
#' denominator= c("M_4_18","18_M_4","4_18_M"),
#' ratios=ratios,
#'  conditions=3,
#' bioreplicates=3,chars = 13,length_labeling =7)
#'}
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom tidyr unite
#' @importFrom stats median
#' @importFrom stringr str_remove
#' @export

make_SILAC_se_parse <- function (proteins_unique,
                                invert,
                                numerator,
                                denominator,
                                columns_positions,
                                ratios,
                                chars = 13,
                                length_labeling =7,
                                sep = "_",
                                conditions=3,
                                bioreplicates=3) {
  
  assertthat::assert_that(is.data.frame(proteins_unique),
                          is.integer(columns_positions),
                          is.character(invert),
                          is.numeric(chars),
                          length(chars) == 1,
                          is.character(sep),
                          length(sep) == 1)
  
  if (any(!c("name") %in% colnames(proteins_unique))) {
    stop("'name' and/or columns are not present in '",
         deparse(substitute(proteins_unique)), "
         '.\nRun make_unique() to obtain the required columns",
         call. = FALSE)    }
  if (any(!apply(proteins_unique[, columns_positions], 2, is.numeric))) {
    stop("specified 'columns' should be numeric",
         "\nRun make_se_parse() with the appropriate columns as argument",
         call. = FALSE)    }
  
  # If input is a tibble, convert to data.frame
  if (tibble::is_tibble(proteins_unique))
    proteins_unique <- as.data.frame(proteins_unique)
  
  ########
  # Select the assay data
  rownames(proteins_unique) <- proteins_unique$name
  raw <- proteins_unique[, columns_positions] 
  raw[raw == 0] <- NA
  raw <- log2(raw)
  raw[str_detect(colnames(raw), invert )] <-  raw[str_detect(colnames(raw),invert)]/-1
  #combine
  raw2<-raw[,str_detect(colnames(raw),str_c(numerator, collapse="|"))]-
    raw[,str_detect(colnames(raw),str_c(denominator, collapse="|"))]
  #substract median
  raw2 <- raw2 %>%
    mutate_all(funs(.- median(.,na.rm = TRUE)))
  rownames(raw2) <-rownames(raw)
  
  #####
 # colnames(raw2) <- str_remove(colnames(raw2),ratios) %>% make.names()
  row_data <- proteins_unique[, -columns_positions]
  rownames(row_data) <- row_data$name
  col_data <- data.frame(label = colnames(raw2), stringsAsFactors = FALSE) %>%
    mutate(condition = substr(label, length_labeling, nchar(label) -  chars), 
           replicate = rep(1:conditions,each=bioreplicates)) %>%
    tidyr::unite(ID, condition, replicate, remove = FALSE)
  rownames(col_data) <- col_data$ID
  colnames(raw2)[match(col_data$label, colnames(raw2))] <- col_data$ID
  raw2 <- raw2[, !is.na(colnames(raw2))]
  se <- SummarizedExperiment::SummarizedExperiment(assays = as.matrix(raw2), colData = col_data, rowData = row_data)
  return(se)
}


