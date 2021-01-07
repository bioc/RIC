#' filter_MaxQuant
#' @param proteins Data.frame,
#' Protein table originating from MaxQuant.
#' @param tofilter Character,
#' Name of the column(s) containing features to be filtered on.
#' @param ratios Character,
#' columns from proteinGroups.txt file that contain the data of interest.
#' @examples
#' if(interactive()){
#' proteins_filtered <- filter_MaxQuant(proteins,
#' tofilter= c("Reverse" , "Potential.contaminant" ,"Only.identified.by.site"),
#'  ratios="Ratio.H.L.normalized.")
#' }
#' @return dataframe
#' @export
filterRIC <- function(proteins, tofilter) {
  assertthat::assert_that(is.data.frame(proteins),
                          is.character(tofilter),
                          is.character(ratios))

  # Get columns
  cols_filt <- match(tofilter, colnames(proteins))

  # Check columns
  if(all(is.na(cols_filt))) {
    warning("No filtering applied\nSpecified tofilter ('",
            paste0(tofilter, collapse = "' and '"),
            "') do not indicate any column",
            call. = FALSE)
  }
  if(any(is.na(cols_filt))) {
    cols_filt <- cols_filt[!is.na(cols_filt)]
  }

  # Filter proteins based on 'tofilter' columns
  message("Filtering based on '", paste(tofilter, collapse = "', '"), "' column(s)")
  if (!is.null(cols_filt)) {
    NAs <- is.na(proteins[,cols_filt])
    proteins[,cols_filt][NAs] <- ""
    if (length(cols_filt) == 1) {
      proteins <- filter(proteins, proteins[,cols_filt] != "+")
    } else if(length(cols_filt) > 1) {
      proteins <- filter(proteins, !apply(proteins[,cols_filt] == "+", 1, any))
    }
  # Filter rows based on valid rows (on at least one valid ratios columns)
    filter(proteins, !apply(proteins[,grep(ratios, colnames(proteins))] == "+", 1, any))

  }
  message("Filtering based on at least one valid value on '", ratios, "' column")
  rowAny <- function(x) rowSums(x) > 0
  proteins <- proteins %>%
    filter(rowAny(across(grep(ratios, colnames(proteins)), ~ !is.na(.x))))

  return(proteins)
}

