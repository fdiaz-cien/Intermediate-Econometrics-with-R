############################### CLASE 2 ########################################

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

data <- wage1 %>% select(wage, educ, exper, tenure, female,married,lwage)


# Analizar los Residuos ------------------------------------------------------

model1 <-lm(wage~educ+exper+tenure+female+married, data=data)
model2 <-lm(lwage~educ+exper+tenure+female+married, data=data)


res1 <- resid(model1) 
plot(density(res1)) 
qqnorm(res1) 
qqline(res1)


#-----------------------------------------------------------------------------#
# Function mutate --- Crea y transforma variables (dplyr)
#-----------------------------------------------------------------------------#

data <- mutate(data, res1)

# ?Presentan sistematicidad los residuos?

p <-data %>% ggplot()+geom_point(aes(wage,res1))
p


# ?Son los residuos normales ? -----------------------------------------------

#-----------------------------------------------------------------------------#
# Function shapiro.test() --- Performs Jarque--Bera test for the composite 
#                           hypothesis of normality, see Jarque and Bera (1987).
#                                                                       
# link: https://www.rdocumentation.org/packages/normtest/versions/1.1/topics/jb.norm.test
#-----------------------------------------------------------------------------#


shapiro.test(data$res1)

#-----------------------------------------------------------------------------#
# Function jb.norm.test() --- Jarque--Bera test for normality (normtest)
#
# link: https://www.rdocumentation.org/packages/normtest/versions/1.1
#-----------------------------------------------------------------------------#

jb.norm.test(data$res1, nrepl=2000)


# Histogramas con la Normal

h <- data %>% ggplot(aes(res1)) +
  geom_histogram(aes(y = stat(density)), binwidth = 0.05, fill = "blue", col = "black") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(data$res1), sd = sd(data$res1)), 
    lwd = 2, 
    col = 'red'
  )

h + ggtitle("Residuos Modelo 1")
h <- h + ggtitle("Residuos Modelo 1")

# QQ-Plots en ggplot

qq <- data %>% ggplot(aes(sample = scale(res1))) + 
  geom_qq() +
  geom_abline()
qq + ggtitle("Residuos Modelo 1")

qq  <- qq + ggtitle("Residuos Modelo 1")

grid.arrange(h, qq, ncol = 2)
grid.arrange(h, qq, nrow = 2)

h  <- h + xlim(-3,5)  # Defino los l?mites a los ejes, de forma 
qq <- qq + xlim(-3,5)  # que sean comunes

#-----------------------------------------------------------------------------#
# Function grid.arrange() --- Permite poner graficos juntos (gridExtra)
#-----------------------------------------------------------------------------#


grid.arrange(h, qq, nrow = 2)

#-----------------------------------------------------------------------------#
# Function grid.newpage() --- Nueva p?gina para los gr?ficos (grid)
# Function grid.draw() ------ Produce un grafico (grid)
#-----------------------------------------------------------------------------#

grid.newpage()
grid.draw(rbind(ggplotGrob(h), ggplotGrob(qq), size = "last"))


# Regresores No Lineales ------------------------------------------------------


model3 <-lm(lwage~educ+exper+tenure+female+married+I(educ^2)+I(exper^2), data=data)
summary (model3)

model4 <-lm(lwage~educ+exper+tenure+female+married+I(exper^2), data=data)
summary (model4)

stargazer(model2,model3,model4, title="Resultados por MCO", align=TRUE,  type="text")
stargazer(model2,model3,model4, title="Resultados por MCO", align=TRUE)

model4a <- lm(educ~I(educ^2) + 0, data=data) #Sin constante
summary (model4a)

# Interaacciones: Variables Discretas ----------------------------------------------

model5 <-lm(lwage~educ+exper+tenure+female+married+exper*female, data=data)
summary (model5)

model6 <-lm(lwage~educ+exper+tenure+female+married+educ*female, data=data)
summary (model6)

stargazer(model5, model6, title="Resultados por MCO", align=TRUE,  type="text")
stargazer(model5, model6, title="Resultados por MCO", align=TRUE)

#-----------------------------------------------------------------------------#
# Function interact_plot() --- Interacciones (interactions)
#-----------------------------------------------------------------------------#


interact_plot(model = model6, pred = educ, modx = female, plot.points = TRUE)

# Interaacciones: Variables Continuas  ---

model7 <-lm(lwage~educ+exper+I(exper^2)+female+educ*exper, data=data)
summary (model7)
stargazer(model7, title="Resultados por MCO", align=TRUE,  type="text")
stargazer(model7, title="Resultados por MCO", align=TRUE)

interact_plot(model = model7, pred = exper, modx = educ, plot.points = TRUE)


############################## FIN CLASE 2 ######################################