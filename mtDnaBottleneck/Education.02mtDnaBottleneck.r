rm(list=ls(all=TRUE))

res = read.table('Education.01mtDnaBottleneck.100.txt')
Agg = aggregate(res[res$FinalHeteroplasmy == 1,]$Generations, by = list(res[res$FinalHeteroplasmy == 1,]$InitialNumberOfMutantAlleles), FUN = median)
names(Agg)=c('InitialHeteroplasmy','Generations')
pdf("Education.01mtDnaBottleneck.100.pdf")
plot(Agg$InitialHeteroplasmy,Agg$Generations, xlab = "initial heteroplasmy: number of neutral alleles out of 500 000", ylab = 'mean number of generations to reach fixation')
cor.test(Agg$InitialHeteroplasmy,Agg$Generations,method='spearman')
dev.off()