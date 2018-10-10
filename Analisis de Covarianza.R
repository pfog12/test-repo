########################
### Clase 08/10/2018 ###
########################

#Establecer directorio de trabajo con el comando "setwd".
setwd("C:/Users/Apodanthaceae/Desktop/Posgrado/Materias/Doctorado/2019-1/Taller de R/Módulo 4")

corteza <- read.csv("DatosCorteza.csv")
View(corteza)

#¿Cómo varía el grosor de la corteza por ambiente? ¿Cuál es el ranking de sitios?
aggregate(corteza$BT, by=list(corteza$Site), mean)
boxplot(corteza$BT~corteza$Site)

plot(corteza$BT~corteza$D)
plot(log(corteza$BT)~log(corteza$D))
#Ver residuos
cor(corteza$D, log(corteza$BT))

#Ahora analiza la relación entre el grosor de la corteza y el tamaño de la rama (diámetro).
plot(corteza$BT~corteza$D)
cor(corteza$BT,corteza$D)
reg.cort <- lm(corteza$BT~corteza$D)
hist(reg.cort$residuals)
plot(reg.cort$residuals~ reg.cort$fitted)###cono
reg.cort2 <- lm(log10(corteza$BT)~log10(corteza$D))
plot(log10(corteza$BT)~log10(corteza$D))
hist(reg.cort2$residuals)
plot(reg.cort2$residuals~ reg.cort2$fitted)

#BT(bark thickness)~sitio+diam

#Compara el grosor medio de la corteza por ambiente.
bartlett.test(corteza$BT~corteza$Site)
bartlett.test(log10(corteza$BT)~corteza$Site) ### no se homogeneizan vars
oneway.test(corteza$BT~corteza$Site)


#Ahora compara el grosor de la corteza controlando por el tamaño de la rama.
#ANCOVA
#Sitio es factor
ancova.corteza<-lm(log10(corteza$BT)~log10(corteza$D)*corteza$Site)
anova(ancova.corteza)
#La interacción no es significativa, por lo que se puede generalizar.
summary(ancova.corteza)

#Compara también los modelos con una prueba de cocientes de verosimilitud. 
#Es posible hacer esta comparación gracias al anidamiento que tienen los modelos.
ancova.corteza <- lm(log10(corteza$BT)~log10(corteza$D)*corteza$Site)
ancova.corteza2<-lm(log10(corteza$BT)~log10(corteza$D)+corteza$Site)
anova(ancova.corteza, ancova.corteza2)

#Si el término de interacción no es significativo, elimínalo del modelo.
ancova.corteza2 <- lm(log10(corteza$BT)~log10(corteza$D)+corteza$Site)
anova(ancova.corteza2)
summary(ancova.corteza2)     

#Se calculan los parámetros de las rectas para cada sitio
# y grafica una recta de diferente color para cada lugar. 
#También identifica los puntos de cada sitio con un color distinto

cols <- c("saddlebrown","tan3","royalblue4","royalblue1", "green4")
puntos <- c(1,16,0,15,2)
plot(log10(corteza$BT)~log10(corteza$D),col=cols[corteza$Site], pch=puntos[corteza$Site])
legend.lab <- c("Eucalypt Woodland","Rainforest", "Savanna", "Scrubland", "Seasonal Forest")
legend("topleft", legend.lab, pch=puntos, col=cols,cex=0.8, bty="n")
###Rectas por ambiente
abline(-1.929, 1.787, col=c("saddlebrown"))###Eucalypt woodland. El problema aquí es que es una recta abarcando dominio extendido de la x.
library(plotrix)
plot(log10(corteza$BT)~log10(corteza$D),col=cols[corteza$Site], pch=puntos[corteza$Site])
legend.lab <- c("Eucalypt Woodland","Rainforest", "Savanna", "Scrubland", "Seasonal Forest")
legend("topleft", legend.lab, pch=puntos, col=cols,cex=0.8, bty="n")
library(plotrix)
ablineclip(a=-1.929,b=1.787, x1=0.7, x2=1.2, col=c("saddlebrown"))###Eucalypt woodland
ablineclip(a=-1.929-0.109902,b=1.787, x1=0.95, x2=1.2, col=c("tan3"))###Rainforest)
ablineclip(a=-1.929+0.115481,b=1.787, x1=1.05, x2=1.4, col=c("royalblue4"))###Savanna)
ablineclip(a=-1.929-0.051837,b=1.787, x1=0.95, x2=1.5, col=c("royalblue1"))###Scrubland)
ablineclip(a=-1.929+0.007333,b=1.787, x1=0.85, x2=1.3, col=c("green4"))###Seasonal Forest)

# Ahora cambiemos el sitio que funge como valor de referencia (EucalyptWoodland, por orden alfabético) para que todas las comparaciones se
# realicen con la el bosque lluvioso
corteza$Site<- relevel(corteza$Site, ref="Rainforest")
ancova.corteza3<- lm(log10(corteza$BT)~log10(corteza$D)+corteza$Site)
anova(ancova.corteza3)
summary(ancova.corteza3)

#Aproximación a través de residuos y ANOVA para resolver el mismo problema
regresion.corteza <- lm(log10(corteza$BT)~log10(corteza$D))
summary(regresion.corteza)
names(regresion.corteza)
summary(regresion.corteza)$residuals
corteza$residuos <- summary(regresion.corteza)$residuals
boxplot(corteza$residuos~corteza$Site)
anova.corteza.residuos <- aov(corteza$residuos~corteza$Site)
summary(anova.corteza.residuos)
TukeyHSD(anova.corteza.residuos)
library(multcompView)
library(lsmeans)
multcompBoxplot(residuos~Site, corteza, horizontal = FALSE, 
                compFn = "TukeyHSD",sortFn = "mean", decreasing = FALSE, 
                plotList = list(boxplot = list(fig =c(0, 0.75, 0, 1)), 
                                                                                                                                     multcompTs = list(fig = c(0.7, 0.85, 0, 1)),multcompLetters = list(fig = c(0.87, 0.97, 0.03, 0.98), fontsize = 20,fontface = "bold")))



class(corteza$Site)
corteza$Site<- as.factor(corteza$Site)

