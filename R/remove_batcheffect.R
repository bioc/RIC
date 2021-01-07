remove_batcheffect <- function (aggregated_peptides, batch2) {
aggregated_peptides[,str_which(names(proteins_prebatch_who),"Intensity")] <-
removeBatchEffect(aggregated_peptides[,str_which(names(proteins_prebatch_who),"Intensity")] 
                                             , batch2) 
}