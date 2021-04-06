Data<-read.delim("clipboard")
Data
names(Data)

#definir los factores
agente<-factor(Data[,1])
rollo<- factor(Data[,2])
resistencia<-Data[,3]

#definir nuestro modelo

mod1<-lm(resistencia~agente+rollo)

#probamos la normalidad
ri<-rstandard(mod1)
shapiro.test(ri)

#Probamos la homogenidad de varianzas con el test de Score

library(car)
library(carData)
ncvTest(mod1)

#Ejercicio 1b
library(agricolae)
summary(aov(mod1))


#Ejercicio 1d
library(multcomp)

anva<-aov(resistencia~agente+rollo)
pk<-glht(anva,linfct=mcp(agente="Tukey"))
summary(pk)         

