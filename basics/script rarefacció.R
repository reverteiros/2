
setwd("D:/Usuarios/s.reverte/Desktop/dades")

DATA<-read.table("interaccions qualitatiu.txt",header=T)

library(vegan)
x<-specaccum(DATA, method = "random", permutations = 100,conditioned =TRUE, gamma = "jack1")

## S3 method for class 'specaccum':
plot(x, add = FALSE, ci = 2, ci.type = c("bar", "line", "polygon"), 
     xlab = "Sites", ylab = x$method)
## S3 method for class 'specaccum':
boxplot(x)


