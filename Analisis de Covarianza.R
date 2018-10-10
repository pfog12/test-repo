##################################
### Título de código apropiado ###
##################################
#--------------------------------------
#Lista de logs:
#--------------------------------------
#Siempre usar el editor y no la consola

#Usar ctrl para enviar líneas a la consola

#Usar # para comentar y agregar títulos y descripciones para
#pasos importantes en el código

#A veces es útil agregar una descripción

#La limpieza y el orden son esenciales
#------------------------------------------
#Cardar datos--------------
#Usar carpetas de forma sistemática con nombres útiles

#usar siempre las mismas:
#Escrito
#input
#output
#R

setwd("C:/Users/Apodanthaceae/Desktop/Posgrado/Materias/Doctorado/2019-1/Taller de R/Módulo 4/test-repo/input")
xlsxfiles<- list.files(path = ".", pattern='*\\.xlsx$')
xlsxfiles

library(openxlsx)
import.list<. lapply(xlsxfiles, read.xlsx, sheet=1, colN)

ls()
working.list<- import.list
names(working.list <- c("db"))
attach(working.list)
       
#---------------------------------------
library(MASS)
library(stats)
#---------------------------------------

#usar espacios

x1<- a+b+c+d
y <- a+b+c

#Usar indets apropiadamente
corplot<- function(x, y, plotit){
  if(plotit == TRUE) plot(x, y)
  cor(x, y)
}

#Guardar en carpeta de output
setwd("C:/Users/Apodanthaceae/Desktop/Posgrado/Materias/Doctorado/2019-1/Taller de R/Módulo 4/test-repo/output")
tiff("test.tiff", width=120, height=120, units="mm")
     corplot(c(2, 5, 6), c(5, 6, 8), TRUE)
     dev.off()
     
---------------------------------
x <- c(0, 1)
#sample nos da números al azar
sample(x, size=1, replace=F)
total.volados <- 20
vec<- vector (length = total.volados)

for(i in 1:total.volados){
  vec[i] <- sample(x, size=1, replace=F)
}

vec
