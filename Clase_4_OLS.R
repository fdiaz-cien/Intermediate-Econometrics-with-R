############################### CLASE 4 ########################################

# **********************************************************************#
#                 Revision Regresion Lineal Multiple                    #                       
# **********************************************************************#

rm(list = ls())

install.packages("dplyr") 
install.packages("car") 

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
library(lubridate)
library(grid)       # Part of package "Graphics" loaded by default



# Cargar Base de Datos--------------------------------------------------

# Supongamos que la base de datos está en formato Excel (vote|.xls)

#-----------------------------------------------------------------------------#
# Function read_excel() ----- Read xls and xlsx files (readxl)
#-----------------------------------------------------------------------------#

#https://www.rdocumentation.org/packages/readxl/versions/1.3.1/topics/read_excel

vote <- readxl::read_excel(path = "/home/fernando/Dropbox/Análisis Econométrico MEET/Data Bases/vote1.xls")
#vote <- readxl::read_excel(path = "C:/Users/Fernando/Dropbox/Análisis Econométrico MEET/Data Bases/vote1.xls")

# O, si tenemos instalado el paquete "readxl":

vote <- read_excel(path = "/home/fernando/Dropbox/Análisis Econométrico MEET/Data Bases/vote1.xls")

# Nótese que no se está leyendo correctamente. La prima fila no tiene los
# nombres de las variables.

#-----------------------------------------------------------------------------#
# Function names() ----- Functions to get or set the names of an object. (base)
#-----------------------------------------------------------------------------#

vote <- read_excel(path = "/home/fernando/Dropbox/Análisis Econométrico MEET/Data Bases/vote1.xls", col_names = FALSE)

# Link a la descripción de la base de datos:
# https://rdrr.io/cran/wooldridge/man/vote1.html

#-----------------------------------------------------------------------------#
# A data.frame with 173 observations on 10 variables:
#
#state: state postal code
#district: congressional district
#democA: =1 if A is democrat
#voteA: percent vote for A
#expendA: camp. expends. by A, $1000s
#expendB: camp. expends. by B, $1000s
#prtystrA: percent vote for president
#lexpendA: log(expendA)
#lexpendB: log(expendB)
#shareA: 100*(expendA/(expendA+expendB))
#-----------------------------------------------------------------------------#

names(vote) <-c("state","district","democA","voteA","expendA","expendB", "prtystrA","lexpendA", "lexpendB", "shareA")
head(vote)
glimpse(vote)
summary(vote)
levels(vote$state)

#-----------------------------------------------------------------------------#
#Function factor() -----The function factor is used to encode a vector as a
#                       factor. (base)
#-----------------------------------------------------------------------------#

levels(factor(vote$state))

# La variable "state" fue codificada en forma bastante extraña. En esta parte
# del códigola recodificamos. El paquete "stringr" debe estat cargado.

vote <- vote %>% mutate(state1 = stringr::str_extract(state, "[A-Z]+"))

# O 

vote <- vote %>% mutate(state = stringr::str_extract(state, "[A-Z]+"))


# Ahora creamos la variable (party), que toma los valores "RP" o "DM", de
# acuerdo a la orientación política del estado respectivo. Vamos  a usar la
# función case_when() y vamos a aprender a usar  %in%.

#-----------------------------------------------------------------------------#
# Operator %in% ----If rather than an index we want a logical that tells us
#                   whether or not each element of a first vector is in a 
#                   second, we can use the function %in%.
#
# The %in% operator is used to identify if an element belongs to a vector.
# Link: http://www.datasciencemadesimple.com/in-operator-in-r/
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
#Function case_when() -----This function allows you to vectorise multiple 
#                          if_else() statements. If no cases match, NA is 
#                         returned.(dplyr)
#-----------------------------------------------------------------------------#



vote <- vote %>% mutate(party=case_when(
  state1 %in% c("CA","CO","CT", "DE", "IL","MA", "MD", "ME", "MI", "NV", "NH","NJ","NM","NY", "OR", "RI","VA", "WA") ~ "DM",
  TRUE ~ "RP")
)
vote <- vote %>% select(-state) %>% rename(state = state1)

# Estad Descriptiva con Stargazer --------------------------------------------

stargazer(vote, type="text", title="Descriptive statistics", digits=1, out="table1.html")
class(vote)
stargazer(as.data.frame(vote), type="text", title="Descriptive statistics", digits=1, out="table1.html")

# Graficos de Dispersion ----------------------------------------------------

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

pairs(~voteA+lexpendA+lexpendB+prtystrA,data=vote,
      main="Matriz de Correlaciones", lower.panel = NULL, panel=panel.lm )


# Gastos de A contra Votos de A: Analisis Grafico ----------------------------

vote %>% ggplot(aes(lexpendA, voteA)) + geom_point() 
vote %>% ggplot(aes(lexpendA, voteA, label=party)) + geom_point() + geom_text()
vote %>% ggplot(aes(lexpendA, voteA, label=party)) + geom_point() + geom_text(check_overlap = TRUE)
vote %>% ggplot(aes(lexpendA, voteA, col=party)) + geom_point() 

# Estimación por OLS ---------------------------------------------------------

model1 <-lm(voteA~lexpendA,data=vote)
model2 <-lm(voteA~lexpendB,data=vote)
model3 <-lm(voteA~prtystrA,data=vote)
model4 <-lm(voteA~lexpendA+lexpendB,data=vote)
model5 <-lm(voteA~lexpendA+lexpendB+prtystrA,data=vote)
stargazer(model1,model2,model3,model4,model5, title="Results", align=TRUE,  type="text", out="table2.html")

cov.matrix<-vcov(model5)  # Esta es la matriz de varianzas y covarianzas
stargazer(cov.matrix, type="text", out="table3.html")

# De los resultados de la estimacion, no pareciese haber problemas de multiolinealidad.

# Elipse de Confianza para Coeficientes Multiples  (CAR package)  ---------

confint(model5, level=0.95)
my.conf <- confint(model5, level=0.95)
class(my.conf) # Como es una matriz, puedo accesar los valores correspondientes
my.conf[2, 1]
my.conf[2, 2]
my.conf[3, 1]
my.conf[3, 2]

linearHypothesis(model5, c("lexpendA=0", "lexpendB=0"))

#-----------------------------------------------------------------------------#
# Function confidenceEllipse() ----- These functions draw ellipses, including 
#                                    data ellipses, and confidence ellipses for 
#                                    linear, generalized linear, and possibly 
#                                    other models. (CAR)
# Link: https://www.rdocumentation.org/packages/car/versions/3.0-7/topics/Ellipses
#-----------------------------------------------------------------------------#


confidenceEllipse(model5, 
                  fill = T,
                  lwd = 0,
                  level=0.95,
                  which.coef = c("lexpendA", "lexpendB"),
                  xlab = "lexpendA",
                  ylab = "lexpendB",
                  main = "95% Confidence Set for the F test: lexpendA=0 & lexpendB=0")

# Intervalos de Confianza de Cada Variable

abline(v=my.conf[2, 1], col="red")
abline(v=my.conf[2, 2], col="red")
abline(h=my.conf[3, 1], col="green")
abline(h=my.conf[3, 2], col="green")

# Agreguemos la Variable Share

model6 <-lm(voteA~lexpendA+lexpendB+prtystrA+shareA,data=vote)

stargazer(model5,model6, title="Results", align=TRUE,  type="text", out="table4.html")

# Nótese que los coeficientes de lexpendA y lexpendB cambian de signo  y pierden significancia.

# Multicolinealidad    ---------------------------------------------------------------

# Alta corrleacion entre las variables explicativas
# Síntomas: R2 altos y tests t bajos
# Es un problema de los datos, no del método de estimación

# Link: http://www.sthda.com/english/articles/39-regression-model-diagnostics/160-multicollinearity-essentials-and-vif-in-r/#loading-required-r-packages

#"For a given predictor (p), multicollinearity can assessed by computing a
#score called the variance inflation factor (or VIF), which measures how much
#the variance of a regression coefficient is inflated due to multicollinearity
#in the model.The smallest possible value of VIF is one (absence of
#multicollinearity). As a rule of thumb, a VIF value that exceeds 5 or 10
#indicates a problematic amount of collinearity (James et al. 2014)."


vif(model5)
vif(model6)

pairs(~lexpendA+lexpendB+shareA,data=vote,
      main="Matriz de Correlaciones", lower.panel = NULL )

model7 <-lm(shareA~lexpendA+lexpendB,data=vote)
stargazer(model7, title="Análisis de Multicolinealidad", align=TRUE,  type="text", out="table5.html")


# Efecto de la Multicolinealidad sobre la Varianza Estimada ------------------------------------------

# Recuerde que Var(bj)= Sigma²/STCj(1-R²j)

model5 <-lm(voteA~lexpendA+lexpendB+prtystrA,data=vote)
model5a <-lm(lexpendA~lexpendB+prtystrA, data=vote)
model6 <-lm(voteA~lexpendA+lexpendB+prtystrA+shareA,data=vote)
model6a <-lm(lexpendA~lexpendB+prtystrA+shareA, data=vote)
stargazer(model5, model5a,model6, model6a, title="Análisis de Multicolinealidad", align=TRUE,  type="text", out="table6.html")


# Nótese que podemos obtner las varianzas "manualmente"

Residuo5     <- residuals(model5)
n5           <- length(Residuo5)
SCR5         <- sum(Residuo5^2)
VarRes5      <- SCR5/(n5-4)
ResStdErr5   <- sqrt(VarRes5)


# La Elipse de Confianza, Test de Restricciones Múltiples y el Efecto de la
# Multicolinealidad -----------------------------------------------------------------------------------


# Modelo Libre
model6 <-lm(voteA~lexpendA+lexpendB+prtystrA+shareA,data=vote)
linearHypothesis(model6, c("lexpendA=0", "lexpendB=0"))
test <- linearHypothesis(model6, c("lexpendA=0", "lexpendB=0"))
stargazer(test, type="text")


# Elipse de Confianza 

confint(model6, level=0.95)
my.conf <- confint(model6, level=0.95)
class(my.conf)
my.conf[2, 1]
my.conf[2, 2]
my.conf[3, 1]
my.conf[3, 2]



confidenceEllipse(model6, 
                  fill = T,
                  lwd = 0,
                  level=0.95,
                  which.coef = c("lexpendA", "lexpendB"),
                  xlab = "lexpendA",
                  ylab = "lexpendB",
                  main = "95% Confidence Set for the F test: lexpendA=0 & lexpendB=0")

# Intervalos de Confianza de Cada Variable

abline(v=my.conf[2, 1], col="red")
abline(v=my.conf[2, 2], col="red")
abline(h=my.conf[3, 1], col="orange")
abline(h=my.conf[3, 2], col="orange")

############################## FIN CLASE 4 ######################################
