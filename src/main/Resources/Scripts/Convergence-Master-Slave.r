resultadoAckley1 <- read.table("master-slave-Ackley-1.csv",header=TRUE)
resultadoAckley2 <- read.table("master-slave-Ackley-2.csv",header=TRUE)
resultadoAckley4 <- read.table("master-slave-Ackley-4.csv",header=TRUE)
plot(seq(1,length(resultadoAckley1$value)),resultadoAckley1$value,type='l',col='red',log='x',xlab="Iterations",ylab="Value",main="Ackley Benchmark")
lines(resultadoAckley2$value,type='l',col='blue')
lines(resultadoAckley4$value,type='l',col='green')
legend("topright", legend=c("1 Worker", "2 Workers","3 Workers"),
       col=c("red", "blue","green"), lty=1, cex=0.8)