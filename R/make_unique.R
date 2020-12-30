#' Make unique names
#'
#' \code{make_unique} generates unique identifiers
#' for a proteomics dataset based on "name" and "id" columns.
#'
#' @param proteins Data.frame,
#' Protein table for which unique names will be created.
#' @param names Character(1),
#' Name of the column containing feature names.
#' @param ids Character(1),
#' Name of the column containing feature IDs.
#' @param delim Character(1),
#' Sets the delimiter separating the feature names within one protein group.
#' @return A data.frame with the additional variables
#' "name" and "ID" containing unique names and identifiers, respectively.
#' @examples
#' if(interactive()){
#' # Load example
#' data <- doxTreatment
#' # Pick the appropriate columns to name proteins uniquely and informatively
#' data_unique <- make_unique(data, "Gene.names", "Protein.IDs", delim = ";")
#' }
#' @export
make_unique <- function (proteins,
                         names,
                         ids,
                         delim = ";") {
  assertthat::assert_that(is.data.frame(proteins),
                          is.character(names),
                          length(names) == 1,
                          is.character(ids),
                          length(ids) == 1,
                          is.character(delim),
                          length(delim) == 1)

   col_names <- colnames(proteins)
   if (!names %in% col_names) {
     stop("'", names, "' is not a column in '", deparse(substitute(proteins)),
          "'", call. = FALSE)    }
   if (!ids %in% col_names) {
     stop("'", ids, "' is not a column in '", deparse(substitute(proteins)),
          "'", call. = FALSE)    }
   if (tibble::is_tibble(proteins))
   proteins <- as.data.frame(proteins)
   double_NAs <- apply(proteins[, c(names, ids)], 1, function(x) all(is.na(x)))
   if (any(double_NAs)) {
     stop("NAs in both the 'names' and 'ids' columns")    }
   proteins_unique <- proteins %>%
     mutate(name = gsub(paste0(delim,         ".*"), "",get(names)),
            ID = gsub(paste0(delim, ".*"),         "", get(ids)),
     name = make.unique(ifelse(name == "" |         is.na(name), ID, name)))
   return(proteins_unique)
   }
