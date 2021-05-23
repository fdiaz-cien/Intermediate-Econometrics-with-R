############################### CLASE 3 ########################################

# **********************************************************************#
#                 Revision Regresion Lineal Multiple                    #                       
# **********************************************************************#

rm(list = ls())

# Cargando las Librerias- Cada Vez

library(tidyverse)
library(readxl)
library(stargazer)
library(normtest)
library(car)
library(wooldridge)
library(interactions)
library(Hmisc) 
library(ggplot2)
library(gridExtra)
library(grid)       # Part of package "Graphics" loaded by default

# Cargar Base de Datos--------------------------------------------------

data <- wage1 

# Intervalos de Confianza--------------------------------------------------

model1 <-lm(lwage~educ+exper+I(exper^2)+female+educ*exper, data=data)
summary (model1)

#-----------------------------------------------------------------------------#
# Function confint() --- Intervalos de Confianzas
#-----------------------------------------------------------------------------#

confint(model1, level = 0.95)


# Tests de Hipótesis Conjunta --------------------------------------------------

# Consideremos la siguiente versión aumentada del modelos de salarios

model1 <-lm(lwage~educ+exper+tenure+female+married+nonwhite+numdep+south+northcen+west, data=wage1)
summary (model1)

# Supongamos que queremos testear que las variables "geográficas" no son
# signficativas para explicar el salario.

#-----------------------------------------------------------------------------#
# Function linearHypothesis() ----- Testea Restricciones Lineales
#-----------------------------------------------------------------------------#

linearHypothesis(model1, c("south=0", "northcen=0","west=0"))

# El test de significancia global lo entrega R por default. Notese que:

class(model1)

# y se pueden acceder sus componentes. Por ejemplo:

# Access the overall F-statistic from the model's summary

summary(model1)$fstatistic

# Supongamos ahora que queremos testear si un año adicional de educación es tan
# valiosos como un año adicional de experiencia.

linearHypothesis(model1, c("educ=exper"))

# O

linearHypothesis(model1, "educ=exper")



# Selección de Variables en Modelos Anidados------------------------------------

# Eliminación Iterativa de Variables Estadśiticamente No Signficativas

# Iteracion 1
model1 <-lm(lwage~educ+exper+tenure+female+married+nonwhite+south+numdep, data=wage1)
summary (model1)
# Eliminamos "numdep"
linearHypothesis(model1, c("numdep=0")) # Notese la relación entre el test t y el 
                                        # test F.
# Iteracion 2
model2 <-lm(lwage~educ+exper+tenure+female+married+nonwhite+south, data=wage1)
summary (model2)
# Eliminamos "nonwhite"
model1 <-lm(lwage~educ+exper+tenure+female+married+nonwhite+south+numdep, data=wage1)
linearHypothesis(model1, c("numdep=0", "nonwhite=0"))

# Iteracion 3
model3 <-lm(lwage~educ+exper+tenure+female+married+south, data=wage1)
summary (model3)
# Eliminamos "south"
model1 <-lm(lwage~educ+exper+tenure+female+married+nonwhite+south+numdep, data=wage1)
linearHypothesis(model1, c("numdep=0", "nonwhite=0", "south=0"))

# Iteracion 4
model4 <-lm(lwage~educ+exper+tenure+female+married, data=wage1)
summary (model4)
# Eliminamos "exper"
model1 <-lm(lwage~educ+exper+tenure+female+married+nonwhite+south+numdep, data=wage1)
linearHypothesis(model1, c("numdep=0", "nonwhite=0", "south=0", "exper=0"))

# Iteracion 5
model5 <-lm(lwage~educ+tenure+female+married, data=wage1)
summary (model5)



stargazer(model1, model2, model3 , model4, model5, title="Resultados por MCO", align=TRUE,  type="text")

############################## FIN CLASE 3 ######################################