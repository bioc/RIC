#' agregagte_singlepeptides
#'
#' @param Qfeature QFeatures,
#' Data object.
#' @param SV_seq AAStringSet,
#' DNAString object.
#' @param ProtFeatures list,
#' ProtFeatures$ProtSeq us a DNAString object.
#' @param whichorder numeric,
#' to correctly reorder labels
#' @param names_samples character,
#' to specify sample names
#' @return a dataframe
#' @examples
#' if(interactive()){
#' aggregatedWCL<-agregate_singlepeptides(QWCLpeptidesfiltered_clean,SV_seq,
#'  whichorder = c(1,2,4,3,6,5,7,10,9,8),
#'   names_samples=   paste(sample_names,rep(1:3,each=3),sep='_'))
#' }
#' @import tibble
#' @import QFeatures
#' @import dplyr
#' @importFrom dplyr rename
#' @export
agregate_singlepeptides <-
  function(Qfeature,
           SV_seq,
           ProtFeatures,
           whichorder,
           names_samples = paste(sample_names, rep(1:3, each = 3), sep = "_")) {
    if (is.integer(whichorder)) {
      whichorder <- as.numeric(whichorder)
    }
    assertthat::assert_that(
      inherits(Qfeature, "QFeatures"),
      inherits(SV_seq, "AAStringSet"),
      is.numeric(whichorder),
      is.character(names_samples)
    )

    if (ncol(assay(Qfeature)) + 1 != length(whichorder)) {
      stop(
        paste0(
          "lenght of whichorder is not valid\n", 
          "Make sure whichorder has length ",
          ncol(assay(Qfeature)) + 1
        ),   call. = FALSE )
    }

    if (ncol(assay(Qfeature)) != length(names_samples)) {
      stop(
        paste0(
          "lenght of names_samples is not valid\n",
          "Make sure names_samples has length ",
          ncol(assay(Qfeature))
        ),     call. = FALSE )
    }

    row_peptides <-
      assay(Qfeature) %>%
      as.data.frame() %>%
      rownames_to_column("sequences")
    row_peptides[, 2:ncol(row_peptides)][row_peptides[, 2:ncol(row_peptides)] <
      1] <- NA
    row_peptides[, 2:ncol(row_peptides)] <- 
      log2(row_peptides[, 2:ncol(row_peptides)])
    # mutate_at(vars(starts_with("Intensity")),log2)
    row_peptides <- row_peptides[, whichorder]

    names(row_peptides) <- c("sequences", names_samples)

    ProtIDs_who <- mapPeptides(
      PeptideSet = row_peptides$sequences,
      c(ProtFeatures$ProtSeq, SV_seq),
      verbose = FALSE
    )
    MapProt2Ensg <- function(protein_list,ProtFeatures){
      lapply(protein_list,function(x)
      {y=unname(ProtFeatures$GeneName[x])
      unique(y[!is.na(y)])})
    }
    ENSGid_who <- MapProt2Ensg(ProtIDs_who,ProtFeatures)
    ENSGid_who[grep("SV_", ProtIDs_who)] <-
      ProtIDs_who[grep("SV_", ProtIDs_who)]
    ENSGid_who[grep("SV_", ProtIDs_who)] <-
      lapply(ENSGid_who[grep("SV_", ProtIDs_who)], function(x) {
        x[1]
      })
    


    ENSGidUnique_who <- ENSGid_who
    ENSGidUnique_who[listLen(ENSGidUnique_who) != 1] <- NA
    ENSGidUnique_who <- unlist(ENSGidUnique_who)
    input_raw_who <-
      cbind(row_peptides,
        ENSGid = ENSGidUnique_who,
        stringsAsFactors = FALSE
      )
    proteins_who <- aggregate(input_raw_who[, 2:ncol(row_peptides)],
      by = list(input_raw_who$ENSGid),
      mean,
      na.rm = TRUE
    ) %>%
      dplyr::rename(ENSGid = Group.1)
    proteins_who$symbol <- (unname(unlist(
      sapply(
        proteins_who$ENSGid,
        function(x) {
          if (any(which(ProtFeatures$GeneName == x))) {
            unique(ProtFeatures$Symbol[which(ProtFeatures$GeneName == x)])
          } else {
            NA
          }
        }
      )
    )))

proteins_who$symbol[grep("SV_", proteins_who$ENSGid)] <-
  as.character(proteins_who$ENSGid[grep("SV_", proteins_who$ENSGid)])
proteins_who$Know_RBP <- ifelse(proteins_who$ENSGid %in%
  enigmRBP$Ensembl.gene.ID,
"known_RBP", "no"
)
    proteins_who <-
      proteins_who %>% select(ENSGid, symbol, Know_RBP, everything())
    proteins_who <- data.frame(proteins_who, stringsAsFactors = FALSE)
    # this could be converted to be returned as a SE object instead
    return(proteins_who)
  }

