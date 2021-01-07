

agregagte_singlepeptides <- function (Qfeature,SV_seq){
  #assettthat...
row_peptides <- assay(Qfeature) %>% as.data.frame()%>% rownames_to_column("Sequence")
ProtIDs_who <- mapPeptides(PeptideSet=row_peptides$Sequence,
                        c(ProtFeatures$ProtSeq,SV_seq),
                        verbose=FALSE)
ENSGid_who  <- MapProt2Ensg(ProtIDs_who)
ENSGid_who[grep('SV_', ProtIDs_who)]  <- 
  ProtIDs_who[grep('SV_', ProtIDs_who)]
ENSGid_who[grep('SV_', ProtIDs_who)]  <- lapply(ENSGid_who[grep('SV_', ProtIDs_who)], function(x) x[1])


ENSGidUnique_who=ENSGid_who
ENSGidUnique_who[listLen(ENSGidUnique_who)!= 1] <- NA
ENSGidUnique_who <- unlist(ENSGidUnique_who)
input_raw_who <- cbind(row_peptides,ENSGid=ENSGidUnique_who, stringsAsFactors=FALSE)


proteins_who <- aggregate(input_raw_who[,2:ncol(row_peptides)], 
                       by=list(input_raw_who$ENSGid), mean, na.rm=TRUE) %>% 
  rename(ENSGid = Group.1)

proteins_who$symbol=(unname(unlist(
  sapply(proteins_who$ENSGid, 
         function(x) {if 
           (any(which(ProtFeatures$GeneName == x))) 
           unique(ProtFeatures$Symbol[which(ProtFeatures$GeneName == x)])
           else NA}))))

proteins_who$symbol[grep('SV_',proteins_who$ENSGid)]=
  as.character(proteins_who$ENSGid[grep('SV_',proteins_who$ENSGid)])

proteins_who$Know_RBP=ifelse(
  proteins_who$ENSGid %in% enigmRBP$Ensembl.gene.ID, 
  'known_RBP', 'no')
proteins_who <- proteins_who %>% select(ENSGid,symbol,Know_RBP, everything())
proteins_who <- data.frame(proteins_who,stringsAsFactors = FALSE)
#this could be converted and returned as a SE object instead
return(proteins_who)

}


