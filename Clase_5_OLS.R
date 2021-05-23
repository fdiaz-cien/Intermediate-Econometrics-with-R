############################### CLASE 5 ########################################

# **********************************************************************#
#                 Revision Regresion Lineal Multiple                    # 
#                  Heterocedasticidad Parte 1                           #
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
library(lubridate) 
library(grid)       # Part of package "Graphics" loaded by default
library(scales)     # Load scales pThe Goldfeld-Quandt ackage for adjusting color opacities



# Un ejmeplo de HAGS------------------------------------------------

# Generate some heteroskedastic data:

# set seed for reproducibility
set.seed(123) 

# set up vector of x coordinates
x <- rep(c(10, 15, 20, 25), each = 25)

# Or we can also code:

x <- rep(c(10, 15, 20, 25), times = c(25,25,25,25))

#-----------------------------------------------------------------------------#
# Function rep() ----- Replicate Elements of Vectors and Lists. (base)
#-----------------------------------------------------------------------------#


# Initialize vector of errors
e <- c()

# sample 100 errors such that the variance increases with x
e[1:25] <- rnorm(25, sd = 10)
e[26:50] <- rnorm(25, sd = 15)
e[51:75] <- rnorm(25, sd = 20)
e[76:100] <- rnorm(25, sd = 25)

# Set up y - FRM
y <- 720 - 3.3 * x + e

# Estimate the model 
mod <- lm(y ~ x)
summary(mod)
confint(mod)

# Plot the data
plot(x = x, 
     y = y, 
     main = "An Example of Heteroskedasticity",
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score",
     cex = 0.5, 
     pch = 19, 
     xlim = c(8, 27), 
     ylim = c(600, 710))

# Add the regression line to the plot
abline(mod, col = "darkred")

# Add boxplots to the plot
boxplot(formula = y ~ x, 
        add = TRUE, 
        at = c(10, 15, 20, 25), 
        col = alpha("gray", 0.4), 
        border = "black")

# We have used the formula argument y ~ x in boxplot() to specify that we want
# to split up the vector y into groups according to x. boxplot(y ~ x) generates
# a boxplot for each of the groups in y defined by x.

# Cargando la Base de Datos ----------------------------------------------------

rm(list = ls())

install.packages("AER") # Applied Econometrics with R (*)

# (*) Functions, data sets, examples, demos, and vignettes for the book Christian
# Kleiber and Achim Zeileis (2008),Applied Econometrics with R, Springer-Verlag,
# New York.

# CPSSWEducation. This data set is part of the package AER and comes from the
# Current Population Survey (CPS) which is conducted periodically by the Bureau
# of Labor Statistics in the United States.

library(AER)
data("CPSSWEducation")
attach(CPSSWEducation)


# Expĺorano la Base de Datos -----------------------------------------------

summary(CPSSWEducation)
head(CPSSWEducation)
stargazer(CPSSWEducation, type="text")

# OLS ---------------------------------------------------------------------


model1 <-lm(earnings~education)
summary(model1)

# Relacion entre Educación e Ingresos

plot(education, 
     earnings, 
     ylim = c(0, 150))
abline(model1, 
       col = "steelblue", 
       lwd = 2)

confint(model1, level = 0.95)

model2 <-lm(earnings~education+gender+age)
summary(model2)

confint(model2, level = 0.95)
coeftest(model2)

# Detectando Heterocedasticidad-----------------------------------------------------------

# Graphical Method

res <- residuals(model2)
yhat <- fitted(model2)
plot(earnings,res, xlab="earnings", ylab="residuals")
plot(yhat,res, xlab="fitted values", ylab="residuals")

# Breush - Pagan Test (*) ---------------------------------------------------------------

# (*) See Chapter 8 Heteroskedasticity, Principles of Econometrics with R
# link: https://bookdown.org/ccolonescu/RPoE4/heteroskedasticity.html


alpha <- 0.05
model <-lm(earnings~education+gender+age)
ressq <- resid(model)^2

# Auxiliar Regression

modres <- lm(ressq~education+gender+age) #Model for the Residuals
summary(modres)
N <- nobs(modres)
glance(modres)
gmodres <- glance(modres)
K <- gmodres$df                         #Number of Betas in model

#Chi-square is always a right-tail test

chisqcr <- qchisq(1-alpha, K-1)         # Critical value
chisqcr
Rsqres <- gmodres$r.squared
chisq <- N*Rsqres                       # R2 of auxiliar regression
pval <- 1-pchisq(chisq,K-1)

print(c(chisq,pval), digits=2)

format(c(chisq,pval), scientific = FALSE, digits=2) # Formating the Output
format(chisq, scientific = FALSE, digits=3)
format(pval, scientific = TRUE, digits=3)

print(c(format(chisq, scientific = FALSE, digits=3),
        format(pval, scientific = TRUE, digits=3)))


# White Test (*) ----------------------------------------------------------------------------

# (*) See Chapter 8 Heteroskedasticity, Principles of Econometrics with R
# link: https://bookdown.org/ccolonescu/RPoE4/heteroskedasticity.html


alpha <- 0.05
model <-lm(earnings~education+gender+age)
ressq <- resid(model2)^2

# Auxiliar Regression

modres <- lm(ressq~education+gender+age+I(education^2)+I(age^2)) #Model for the Residuals
summary(modres)
N <- nobs(modres)
glance(modres)
gmodres <- glance(modres)
K <- gmodres$df                         #Number of Betas in model


#Chi-square is always a right-tail test

chisqcr <- qchisq(1-alpha, K-1)         # Critical value
chisqcr
Rsqres <- gmodres$r.squared
chisq <- N*Rsqres                       # R2 of auxiliar regression
pval <- 1-pchisq(chisq,K-1)
print(c(format(chisq, scientific = FALSE, digits=3),
        format(pval, scientific = TRUE, digits=3)))


# The Goldfeld-Quandt test ----------------------------------------------------------------------------


sample <- CPSSWEducation %>% arrange(education) %>% mutate(quantile=ntile(education,10)) # Creamos deciles de "education"

alpha <- 0.05

# Subsample 1

subsample1 <- sample %>% filter(quantile <=5)
model <-lm(earnings~education+gender+age, data=subsample1)
summary(model)
N1 <- nobs(model)
glance(model)
gmodel <- glance(model)
K <- gmodel$df    
s1 <-gmodel$sigma
v1 <- s1^2

# Subsample 2

subsample2 <- sample %>% filter(quantile >= 6)
model <-lm(earnings~education+gender+age, data=subsample2)
summary(model)
N2 <- nobs(model)
glance(model)
gmodel <- glance(model)
K <- gmodel$df    
s2 <-gmodel$sigma
v2 <- s2^2

if (s2 > s1) {
        dfnum <- N2 - K
        dfden <- N1 - K
} else {
        dfnum <- N1 - K
        dfden <- N2 - K
}

F_hat <- max(v1,v2)/min(v1,v2)
Fc <- qf(1-alpha, dfnum, dfden) #Right (upper) critical F

print(paste(c("F test","Critical Value"), c(format(F_hat, scientific = FALSE, digits=3),
        format(Fc, scientific = FALSE, digits=3))))

# Tests de Heterocedasticidad de R  -----------------------------------------------

install.packages("kableExtra")
library(knitr)
library(kableExtra)

model <-lm(earnings~education+gender+age, data = CPSSWEducation)
summary(model)

# Breush Pagan
bptest(model)
kable(tidy(bptest(model)), 
      caption="Breusch-Pagan heteroskedasticity test", "rst")


# Goldfeld y Quandt
GQ <- gqtest(model, point = 0.5, alternative="greater",
              order.by= CPSSWEducation$education)

kable(tidy(GQ), 
      caption="R function `gqtest()`", "rst")


# Robust Standard Errors ----------------------------------------------------------

rm(list = ls())

library(AER)
data("CPSSWEducation")
attach(CPSSWEducation)

model <-lm(earnings~education+gender+age, data = CPSSWEducation)
summary(model)

# Matriz de Varianzas Covarianzas

vcov            <- vcovHC(model)
vcovWhite       <- vcovHC(model, type = "HC0")
vcov
vcovWhite

#-----------------------------------------------------------------------------------------#
# Function coeftest() --- Performs z and t Wald tests. It allows for specification of the  
#                         variance covariance matrix wit the vcov. option.
#                         Type = "HC0" computes White st errors (good for large sample).
#                         HC1, HC2, HC3 were suggested by MacKinnon and White (1985) to 
#                         improve the performance in small samples.
#                         HC4 was suggested by Cribari-Neto (2004) to further improve small 
#                         sample performance, especially in the presence of influential 
#                         observations.For details, see:
# https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf
#-----------------------------------------------------------------------------------------#

# t tests

coeftest(model)
coeftest(model, vcov. = vcovHC, type = "HC0") #White SEs
coeftest(model, vcov. = vcovHC, type = "HC4") #Cribari-Neto SEs

# F tests

linearHypothesis(model, c("education=0","age=0"), white.adjust = "hc0")

# Intervalos de Confianza

ct      <- coeftest(model)
ctwhite <- coeftest(model, vcov. = vcovHC, type = "HC0") 

confint(ct)
confint(ctwhite)



############################## FIN CLASE 5 ######################################