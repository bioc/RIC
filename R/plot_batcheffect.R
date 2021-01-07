plot_batcheffect <- function (aggregated_peptides) {

MDS_who=plotMDS(log2(aggregated_peptides[,str_which(colnames(aggregatedRIC),"Intensity")]),
                labels = NULL, pch= c(19), col= c(3:1), cex = 1,
                gene.selection = "pairwise", main='Multidimensional scaling Input',
                xlab = "Leading logFC dimension 1", 
                ylab = "Leading logFC dimension 2")
text(x=MDS_who$x, y = MDS_who$y, labels=colData(QWCLpeptides)$sample, pos= 4, col=c(3:1))
legend("topright", legend=colData(QWCLpeptides)$group[1:3], col=3:1, pch=19)

}


