#' plot_singlepeptides
#'
#' @param Qfeature QFeatures,
#' Data object.
#' @param SV_seq AAStringSet,
#' DNAString object.
#' @param ProtFeatures list,
#' ProtFeatures$ProtSeq us a DNAString object.
#' @param ENSG Logical,
#' Whether or not peptide-mapping proteins are mapped to genes.
#' @param plot Logical(1),
#' If \code{TRUE} (default) the stacked barplot is produced.
#' @examples
#' if(interactive()){
#' plot_singlepeptides(QWCLpeptidesfiltered_clean,SV_seq,ProtFeatures)
#' }
#' @return a stacked bar chart
#' @import purrr
#' @export
plot_singlepeptides <-
  function(Qfeature,
           SV_seq,
           ProtFeatures,
           ENSG = TRUE,
           plot = TRUE) {
    assertthat::assert_that(
      inherits(Qfeature, "QFeatures"),
      inherits(SV_seq, "AAStringSet"),
      is.logical(ENSG),
      is.logical(plot)
    )

    row_peptides <-
      rowData(Qfeature[["peptides"]]) %>% as.data.frame()
    ProtIDs_who <- mapPeptides(
      PeptideSet = row_peptides$Sequence,
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
    ENSGid_who[grep("SV_s", ProtIDs_who)] <-
      lapply(ENSGid_who[grep("SV_", ProtIDs_who)], function(x) {
        x[1]
      })

    if (ENSG) {
      df <- map_dbl(ENSGid_who, length) %>%
        table() %>%
        as.data.frame()
    } else {
      df <- map_dbl(ProtIDs_who, length) %>%
        table() %>%
        as.data.frame()
    }
    colnames(df) <- c("nr_protein_matches", "nr_peptides")

    p <-
      ggplot(df, aes(x = "", y = nr_peptides, fill = nr_protein_matches)) +
      geom_col(col = "white") +
      labs(
        title = "Peptides mapping",
        x = "",
        y = "Number of peptides",
        fill = "Gene matches"
      ) +
      theme_bw()
    if (plot) {
      return(p)
    } else {
      return(df)
    }
  }

