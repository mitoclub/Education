# what is the rate of the origin of new mutations per genome per generation in order to contribute 1-2 de novo per genome per generation with freq > 0.01
# add 1 de novo neutral mutation and see how many of them will reach > 1%?
# Makova can see de novo mutations with frequency > 1%. If bottleneck Ne is just 25 it means that new mutant (with initial freq = 1/500000) should be lucky to be among 25
# In other words, in order to see 1-2 de novo mutations per genome per generation we need to introduce much more real mutations, vast majority of which will disappear immediately
# if there is one mutation per genome per generation - one per 25 molecules should have one new mutation - .i.e. 100000/25 ~ 4000 new mutations should happen in oocyte!

rm(list=ls(all=TRUE))

library(dplyr)

### main parameters
BottleneckSize = 25         
NumberOfMtDnaPerOocyte = 100000
# NumberOfIterations = 100 # 500
NumberOfGenerations = 1000

##### 0: initialization of a population with no mutations:
GenomeId=seq(1,NumberOfMtDnaPerOocyte); length(GenomeId)
Population = data.frame(GenomeId)
Population$MutPos = ''
MitoGenomePos = seq(1,16500)
MitoPopulationId = seq(1,NumberOfMtDnaPerOocyte)
MutRate = 4000 # number of de novo mtDNA mutations per whole mtDNA population in oocyte
DeNovoInherited = MutRate/NumberOfMtDnaPerOocyte
AllMut=data.frame()

for (i in 1:NumberOfGenerations)
{ # i = 1
  ##### 1: de novo mutagenesis:
  for (mut in 1:MutRate)
  {
  RandomGenome =  sample(MitoPopulationId,1)
  RandomPosition = sample(MitoGenomePos,1)
  Population$MutPos[RandomGenome] = paste(Population$MutPos[RandomGenome],RandomPosition,sep='.')
  }
  
  ### 2 bottleneck:
  Population = sample_n(Population,BottleneckSize)
  
  ### 3 expansion:
  Population = sample_n(Population,NumberOfMtDnaPerOocyte,replace = TRUE)
  
  MutVec = paste(Population$MutPos,collapse = '')
  MutVec = unlist(strsplit(MutVec,'\\.'))
  MutVec = data.frame(table(MutVec)); 
  if (nrow(MutVec) > 0) 
    {
    names(MutVec)=c('MutPos','Freq')
    MutVec = MutVec[MutVec$MutPos != '',]
    MutVec$Gener = i
    AllMut = rbind(AllMut,MutVec)
  }
  gc()  
}

### mutations
AllMutVec = paste(Population$MutPos,collapse = '')
AllMutVec = unlist(strsplit(AllMutVec,'\\.'))
length(AllMutVec)
AllMutVec = data.frame(table(AllMutVec)); 
names(AllMutVec)=c('MutPos','Freq')
AllMutVec = AllMutVec[AllMutVec$MutPos != '',]

write.table(AllMut,'Education.03mtDnaBottleneck.AllMut.txt') 
# next - I need to analyze this file and see equilibrium, frequencies etc...





