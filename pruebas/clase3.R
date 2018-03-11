##Clase 3
set.seed(1997)
hijos<-rpois(n=400, lambda = 4)
hist(hijos, breaks = 9)
crias<-rpois(n=400, lambda = 300)#lambda=promedio de crias
hist(crias, breaks = 9)

rnorm(n=100, mean = 0, sd = 1)

matnorm<- matrix(, nrow = 1000, ncol = 100)
for(i in 1:1000){
  matnorm[i,]<- rnorm(n=100, mean = 65, sd = 8)
}
head(matnorm)
meansnorm<- apply(matnorm, 1, mean)# 1: filas , 2: columnas
str(meansnorm)
hist(meansnorm)
matreduced<- matnorm[1:3,]
par(mfrow=c(1,1))
apply(matreduced, 1, hist)

matpois<- matrix(, nrow = 1000, ncol = 100)
for(i in 1:1000){
  matpois[i,]<- rpois(n=100, lambda = 2.1)
}
head(matpois)
meanspois<- apply(matpois, 1, mean)# 1: filas , 2: columnas
str(meanspois)
hist(meanspois)
matreduced<- matpois[1:3,]
par(mfrow=c(1,1))
apply(matreduced, 1, hist)
