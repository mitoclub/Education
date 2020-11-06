rm(list=ls(all=TRUE))

SelCoef = 0.001           # how deleterious is the mutation (SDV)
EpistCoef = 0            # if zero => additive, if > 0 positive epistasis 
GenomeContam = seq(1,1000,1) # Genome - wide burden of slightly - deleterious variants (SDV)
Fitness = (1-SelCoef)^GenomeContam - EpistCoef*GenomeContam^2
# the probability to survive with one mutation = 1-SelCoeff

par(mfcol=c(2,2))

### ADDITIVE:
plot(GenomeContam,Fitness)
plot(GenomeContam,log(Fitness))


EpistCoef = 0.00001
Fitness = (1-SelCoef)^GenomeContam - EpistCoef*GenomeContam^2
# the probability to survive with one mutation depends ALSO on the total number of all mutations: - EpistCoef^(GenomeContam*GenomeContam)

### EPISTATIC:
plot(GenomeContam,Fitness)
plot(GenomeContam,log(Fitness))