library(ggplot2)
Resultados <- read.table("secStatisticsQuadric.csv",header=TRUE)
ggplot(Resultados,aes(x=c("Quadric"),y=value),show.legend = FALSE)+geom_violin(trim=FALSE)+geom_jitter(position=position_jitter(0.2))+labs(x="Benchmark", y = "Value")+theme_classic()+ theme(legend.position="none")
