resultadoQuadric <- read.table("secQuadric.csv",header=TRUE)
plot(seq(1,length(resultadoQuadric$value)),resultadoQuadric$value,type='l',col='red',log='x',xlab="Iterations",ylab="Value",main="Ackley Benchmark")
