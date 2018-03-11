set.seed(1977)

edad<- runif(50, min = 11, max = 13)
raza<- sample(c("Blanco","Negro","Mestizo"),50, replace = T)
sexo<- rbinom(50,1,prob = 0.5)
peso<- vector(mode = "numeric", length = 50)
bd<- data.frame(edad,raza,sexo,peso)
bd$peso <- rnorm(50, mean = (3.5*edad)- bd$sexo, sd = 5- bd$sexo/2)