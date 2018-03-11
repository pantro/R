set.seed(1977)
edad<-runif(50,min=11,max=13)
raza<-sample(c("caucasico","afroperuano","mestizo"),50,replace=T)
sexo<-rbinom(50,1,prob=0.5)#Femenino=1;Masculino=0
peso<-vector(mode="numeric",length=50)

bd<-data.frame(edad,raza,sexo,peso)
bd$peso<-rnorm(50,mean=(3.5*edad)-bd$sexo,sd=5-bd$sexo/2)
hist(bd$peso, breaks=5,freq = F)
boxplot(bd$peso, horizontal = T)
plot(bd$edad, bd$peso)
bd$edad12[bd$edad<12]<-0
bd$edad12[bd$edad>=12]<-1
head(bd)
plot(bd$edad12, bd$peso)
table(bd$edad,bd$edad12)
plot(bd$edad12+rnorm(length(bd$edad12), mean = 0.1,
                     sd = 0.05),bd$peso)
scatter.smooth(bd$edad, bd$peso)
library(modeest)
mlv(bd$raza, method = "mfv")
hist(bd$raza)
barplot(table(bd$raza))
par(las=1)
boxplot(bd$edad ~ bd$sexo,ylab="Peso (Kg.)", xlab="Sexo",
        names=c("Hombres","Mujeres"), 
        main="Relacion entre peso y sexo")
mtext("Escolares de 11 a 13 anhos - Arequipa", cex=1)

bd[1:3, c("peso","edad")]
summary(bd$edad)
str(bd)
unique(bd$raza)
unique(bd$peso)
round(bd$peso, digits=1)
bd$pesoround <- round(bd$peso, digits=2)

pesoRicardo<- rnorm(12, 65, 2)
meses <- c(1:12)
round(bd$peso, digits=1)
plot(meses, pesoRicardo, type = "o", axes = F, 
     xlab = "Meses", ylab = "Peso (Kg.)")
axis(side = 1, at=c(1:12),
     labels = c("E","F","M","A","M","J","J","A","S","O","N","D"))
axis(side = 2, at=c(62,64,66,68,70),
     labels = c(62,64,66,68,70))
