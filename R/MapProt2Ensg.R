#' MapProt2Ensg
#'
#' @param protein_list list
#' list of proteins 
#' @examples
#' if(interactive()){
#' ENSGid_who = MapProt2Ensg(ProtIDs_who)
#' }
#' @return a list of mapped genes
#' 
#' @export
MapProt2Ensg <- function(protein_list){
  lapply(protein_list,function(x)
  {y=unname(ProtFeatures$GeneName[x])
  unique(y[!is.na(y)])})
}