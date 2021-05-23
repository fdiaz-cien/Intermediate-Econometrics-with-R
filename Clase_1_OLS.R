#################   Clase 1: OLS Basics #################################

# **********************************************************************#
#                 Revision Regresion Lineal Multiple                    #                       
# **********************************************************************#


rm(list = ls())
installed.packages() # Which packages I have installed

  # Cargando los Paquetes- Solo una Vez
install.packages("stargazer")    #For exporting tables
install.packages("normtest")     #For normality tests
install.packages("car")          #For robust standard errors
install.packages("wooldridge")   #For Book Data Sets
install.packages("interactions") #For interactions graphs
install.packages("Hmisc")        #For interactions graphs
install.packages("tidyverse")

# Cargando las Librerias- Cada Vez

library(tidyverse)
library(readxl)
library(stargazer)
library(normtest)
library(car)
library(wooldridge)
library(interactions)
library(Hmisc) #Correlation
library(ggplot2)
library(gridExtra)



# *********************************************************************#
# -------------Analisis con Multiples Variables -----------------------#
# *********************************************************************#


# Cargar Base de Datos--------------------------------------------------

data <- wage1
names(data)

# Seleccionando los Datos para el Analisis-----------------------------

data <- select(data,wage, educ, exper, tenure, female,married,lwage)
data <- data %>% select(wage, educ, exper, tenure, female,married,lwage)

head(data)
glimpse(data)
summary(data)

# Stargazer formatea tablas y resultados de regresion-----------------


stargazer(data)
stargazer(data, type="text")
stargazer(as.data.frame(data))  # For latex
stargazer(as.data.frame(data), type="text")

stargazer(as.data.frame(data), type="text", 
         title="Descriptive statistics", digits=2, out="Estadistica Descriptiva.txt")

stargazer(as.data.frame(data), type="text", 
          title="Descriptive statistics", digits=2, out="Estadistica Descriptiva.html")

#--------------------------------------------------------------------------------#
#link: https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf
#--------------------------------------------------------------------------------#

# Graficos de Dispersion-------------------------------------------------

# Siempre es importante mirar las correlacioenes entre las variables. 

pairs(~wage+educ+exper+tenure+female+married,data=data,
      main="Matriz de Correlaciones")

pairs(~wage+educ+exper+tenure+female+married,data=data,
      main="Matriz de Correlaciones", lower.panel = NULL )

panel.lm <- function (x, y,  pch = par("pch"), col.lm = "red") {   
  ymin <- min(y)
  ymax <- max(y)
  xmin <- min(x)
  xmax <- max(x)
  ylim <- c(min(ymin,xmin),max(ymax,xmax))
  xlim <- ylim
  points(x, y, pch = pch,ylim = ylim, xlim= xlim)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    abline(lm(y[ok]~ x[ok]), 
           col = col.lm)
}


pairs(~wage+educ+exper+tenure+female+married,data=data, panel=panel.lm)
pairs(~wage+educ+exper+tenure+female+married,data=data,
      main="Matriz de Correlaciones", lower.panel = NULL, panel=panel.lm )

#--------------------------------------------------------------------------#
# Function corr() --- Returns correlation coefficents
#--------------------------------------------------------------------------#


correl1 <- cor(data, method = "pearson")
correl1
correl1 <-round(correl1,2)

correl1 <- data %>% select(-lwage) %>% cor(method = "pearson") %>% round(2)
correl1

#-----------------------------------------------------------------------------#
# Unfortunately, neither cor( ) or cov( ) produce tests of significance,
# although you can use the cor.test( ) function to test a single correlation
# coefficient. The rcorr( ) function in the Hmisc package produces
# correlations/covariances and significance levels for pearson and spearman
# correlations. However, input must be a matrix and pairwise deletion is used.

#Source: https://www.statmethods.net/stats/withby.html
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# Function rcorr() --- Returns correlation coefficents and p values
#-----------------------------------------------------------------------------#


correl2 <- rcorr(as.matrix(select(data,-lwage), type=c("spearman")))
correl2

# OLS --------------------------------------------------------------------------

#-----------------------------------------------------------------------------#
# Function lm() --- Linear Model
#-----------------------------------------------------------------------------#


model1 <-lm(wage~educ+exper+tenure+female+married, data=data)
summary (model1)

model2 <-lm(lwage~educ+exper+tenure+female+married, data=data)
summary (model2)

stargazer(model1,model2, title="Resultados por MCO", align=TRUE,  type="text")
stargazer(model1,model2, title="Resultados por MCO", align=TRUE)

# Analizar los Residuos ------------------------------------------------------

res1 <- resid(model1) 
plot(density(res1)) 
qqnorm(res1) 
qqline(res1)

#-----------------------------------------------------------------------------#
# Function mutate --- Crea y transforma variables (dplyr)
#-----------------------------------------------------------------------------#

data <- mutate(data, res1)

# ?Son los residuos normales ?

#-----------------------------------------------------------------------------#
# Function shapiro.test() --- Performs Jarque--Bera test for the composite 
#                           hypothesis of normality, see Jarque and Bera (1987).
#                                                                       
# link: https://www.rdocumentation.org/packages/normtest/versions/1.1/topics/jb.norm.test
#-----------------------------------------------------------------------------#


shapiro.test(data$res1)

#-----------------------------------------------------------------------------#
# Function jb.norm.test() --- arque--Bera test for normality (normtest)
#
# link: https://www.rdocumentation.org/packages/normtest/versions/1.1
#-----------------------------------------------------------------------------#

jb.norm.test(data$res1, nrepl=2000)


############################## FIN CLASE 1 ######################################



