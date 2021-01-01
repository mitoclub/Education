rm(list=ls(all=TRUE))

res = read.table('Education.01mtDnaBottleneck.100.txt')
Agg = aggregate(res[res$FinalHeteroplasmy == 1,]$Generations, by = list(res[res$FinalHeteroplasmy == 1,]$InitialNumberOfMutantAlleles), FUN = median)
names(Agg)=c('InitialHeteroplasmy','Generations')
plot(Agg$InitialHeteroplasmy,Agg$Generations)
cor.test(Agg$InitialHeteroplasmy,Agg$Generations,method='spearman')