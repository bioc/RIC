




extract_singlepeptides <- function (Qfeature,SV_seq, ENSG=TRUE,plot=TRUE) {
  #assettthat...
row_peptides<-rowData(Qfeature[["peptides"]]) %>% as_data_frame()
ProtIDs_who=mapPeptides(PeptideSet=row_peptides$Sequence,
                          c(ProtFeatures$ProtSeq,SV_seq),
                          verbose=FALSE)
ENSGid_who=MapProt2Ensg(ProtIDs_who)
ENSGid_who[grep('SV_', ProtIDs_who)]=
ProtIDs_who[grep('SV_', ProtIDs_who)]
ENSGid_who[grep('SV_', ProtIDs_who)]=lapply(ENSGid_who[grep('SV_', ProtIDs_who)], function(x) x[1])

if(ENSG) {
df=map_dbl(ENSGid_who,length) %>% table() %>% as.data.frame()
}else{
df=map_dbl(ProtIDs_who,length) %>% table() %>% as.data.frame()
  
}
colnames(df)=c('nr_protein_matches','nr_peptides')

p <- ggplot(df, aes(x = "", y = nr_peptides, fill = nr_protein_matches)) +
  geom_col(col = "white")+
  labs(title = "Peptides mapping",
       x = "",
       y = "Number of peptides",
       fill = "Gene matches")+
  theme_bw()
if(plot) {
  return(p)
} else {
  return(df)
}

}

