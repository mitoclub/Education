rm(list=ls(all=TRUE))

### main parameters
BottleneckSize = 25         
NumberOfMtDnaPerOocyte = 500000
NumberOfIterations = 100 # 500
results = data.frame(); 
for (mut in 0:25)
{
  if (mut > 0) {NumberOfMutantAlleles = mut*10000}
  if (mut == 0) {NumberOfMutantAlleles = 1}

  for (iter in 1:NumberOfIterations)
    {
    Heteroplasmy = NumberOfMutantAlleles / NumberOfMtDnaPerOocyte
    Generation = 1
    CurrentNumberOfMutantAlleles = NumberOfMutantAlleles

    while (Heteroplasmy > 0 & Heteroplasmy < 1)
      {
      Genotype = c(rep(1,CurrentNumberOfMutantAlleles),rep(0,NumberOfMtDnaPerOocyte - CurrentNumberOfMutantAlleles))
      table(Genotype)
      ### after bottleneck:
      NextGeneration = sample(Genotype,BottleneckSize)
      NextOocyte = rep(NextGeneration, NumberOfMtDnaPerOocyte/BottleneckSize); length(NextOocyte) # we just multiply! in reality there is some stochasticity!
      CurrentNumberOfMutantAlleles = length(NextOocyte[NextOocyte == 1])
      Heteroplasmy = CurrentNumberOfMutantAlleles / NumberOfMtDnaPerOocyte
      Generation = Generation + 1
      # print(c(Generation,Heteroplasmy))
      }
    results = rbind(results,c(BottleneckSize,NumberOfMtDnaPerOocyte,NumberOfMutantAlleles,Heteroplasmy,Generation))
    }
}  
names(results)=c('BottleneckSize','NumberOfMtDnaPerOocyte','InitialNumberOfMutantAlleles','FinalHeteroplasmy','Generations')

output = paste('Education.01mtDnaBottleneck.',NumberOfIterations,'.txt',sep = '')
write.table(results,output)

median(results[results$FinalHeteroplasmy == 0,]$Generations)
median(results[results$FinalHeteroplasmy == 1,]$Generations)

hist(results[results$FinalHeteroplasmy == 1,]$Generations, breaks = 50)
hist(results[results$FinalHeteroplasmy == 0,]$Generations, breaks = 50)
