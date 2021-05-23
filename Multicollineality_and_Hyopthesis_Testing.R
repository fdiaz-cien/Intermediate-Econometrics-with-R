# ************  Instalando Paquetes                ********************


install.packages('tidyverse')
install.packages('gridExtra')
install.packages('car')

library(tidyverse)
library(gridExtra)
library(car)
library(lmtest)
library(dplyr)

rm(list = ls()) # Limpia el Environment

# ************   Generar la Funcion de Regrecsion Poblacional  ********************#

# FRP:  y = b1 + b2*x2 + b3*x3 + u

b <- c(5,15,10)  # Definiendo parametros poblacionales
N <- 10000       # Tamaño de la poblacion

# Generate Xs and Errors

x2 <- runif(N, min = 10, max = 20)
#x3 <- runif(N, min = 5, max = 10)

#--------------------------------------------------------------------#
# Estos son los valores para x3 con colinealidad 
e <- rnorm(N,mean=0, sd=1)
x3 <- 0.5*sqrt(x2)+ e
#--------------------------------------------------------------------#

u <- rnorm(N,mean=0, sd=25) # Error

y<-vector(length = N)      # Variable Dependiente

for (i in 1:N){
  y[i]<-b[1]+b[2]*x2[i]+b[3]*x3[i]+u[i]
}

summary(y)

# *************   Creamos un Data Frame  ******************************************#

df<-data.frame(y,x2,x3)
head(df)

# *************   Generamos una  Muestra de tamaño n = 1000 ************************#

sample <-sample_n(df, 1000)

# *************   Estimamos el Model ************************#

linear_model <- lm(y ~ x2+x3, data = sample)
summary(linear_model)



# *************   Tests Individuales y Conjuntos  ************************#

my.conf <- confint(linear_model, level=0.95)
my.conf
li_x2 <- my.conf[2, 1]
ls_x2 <- my.conf[2, 2]
li_x3 <- my.conf[3, 1]
ls_x3 <- my.conf[3, 2]

# *************  Grid de Valores  ************************#

values_x2      <- seq(li_x2, ls_x2, length.out = 100)
values_x3      <- seq(li_x3, ls_x3, length.out = 100)
incongruencia  <- matrix(, nrow=100,ncol=100) 

for (i in 1:100){
  for (j in 1:100){
    
    lh               <- linearHypothesis(linear_model, c("x2", "x3"), c(values_x2[i],values_x3[j]))
    p_lh             <- lh[2,6]               # Valor p del test F Conjunto
    incongruencia[i,j] <- ifelse(p_lh < 0.05,1,0)
  }
}

mean(incongruencia)

confidenceEllipse(linear_model, 
                  fill = T,
                  lwd = 0,
                  level=0.95,
                  which.coef = c("x2", "x3"),
                  xlab = "x2",
                  ylab = "x3",
                  main = "95% Confidence Set for x2 & x3")

abline(v=li_x2, col="red")
abline(v=ls_x2, col="red")
abline(h=li_x3, col="green")
abline(h=ls_x3, col="green")

