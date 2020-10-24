#' Endogeneity & Instrumental Variables 
#' Data: HealthInsurance.csv (10,090 x 30)
#' 
#' Problem: We want to study the relationship between healthins and medexpense, 
#' i.e., whether having health insurance reduces medical expense

setwd("C:/Users/abhatt/Desktop/SDM/Data")
d <- read.csv("HealthInsurance.csv")
attach(d)

#' Visualizations

hist(d$medexpense, breaks=20, prob=T, main="Histogram of medexpense") 
den <- density(d$medexpense)                    
lines(den, col="red")

hist(d$logmedexpense, breaks=20, prob=T, main="Histogram of logmedexpense") 
den <- density(d$logmedexpense)
lines(den, col="red")

hist(d$income, breaks=20, prob=T)
hist(d$logincome, breaks=20, prob=T)

#' Variable definition

Y1 <- cbind(logmedexpense)                  # Dependent variable
Y2 <- cbind(healthins)                      # Endogeneous variable - health insurance will be y in first model and then x in the second
X1 <- cbind(illnesses, age, logincome)      # Exogeneous variable (controls)
IV <- cbind(ssiratio)                       # Instrumental variable (related to logincome)
IValt <- cbind(ssiratio, married)           # Instrumental variables, overidentified

summary(Y1)
summary(Y2)
summary(X1)
summary(IV)

#' OLS regression

ols <- lm(Y1 ~ Y2 + X1)
summary(ols)

#' Finding: OLS model shows that healthins INCREASES medexpense by 7.5% (note: log scale), when 
#' controlled for illnesses, age, and logincome. Is this is so, why would anyone buy healthins? 
#' Let's check for endogeneity using an instrumental variable and two stage least squares regression.

#' 2SLS regression
#' Stage 1: Regress endogeneous variable against exogeneous and instrumental variables
#' Stage 2: Regress dependent variable against exogeneous variables and fitted values from Stage 1

ols1 <- lm (Y2 ~ X1 + IV)                   
summary(ols1)
Y2hat <- fitted(ols1)

ols2 <- lm(Y1 ~ Y2hat + X1)
summary(ols2)

#' Finding: healthins REDUCES medexpense by 85.2%, when controlled for illnesses, age, and logincome,
#' and corrected for the endogeneous effects of ssiratio.

#' The two steps can be combined into a single equation using the ivreg() function in the AER package.
#' Note: In ivreg() syntax, exogeneous variables X1 appears on both sides of the |, endogeneous
#' variable Y2 appears only to the left of the |, and the IV appears only on the right.

install.packages("AER")
library(AER)
ivreg <- ivreg(Y1 ~ Y2 + X1 | X1 + IV)
summary(ivreg)

# we can use ivreg(), tsls(), iv-robust() - all give similiar results

#' Finding: Results are exacttly the same as doing it is two separate stages.
#' 
#' A proper instrumental variable is the one that impacts dependent variable of first model and not the second one.

#' 2SLS estimation, over-identified case

ivreg2 <- ivreg(Y1 ~ Y2 + X1 | X1 + IValt)
summary(ivreg2)

#' its atleast in the correct direction. married is a good instrumental variable 
#' employed might be a potential variable as some employers get insurance benefits
#' firm size might have an impact too - we can try that

library(stargazer)
stargazer(ols, ols2, ivreg, ivreg2, type="text")

#' Finding: Effect decrease quite a bit compared to the previous model but remains negative.

#' Hausman test for endogeneity
#' Comparison of coefficients between the OLS and 2SLS models. If coefficients are close, 
#' then there is no endogeneity, and we don't need 2SLS.
#' H0: No endogeneity between OLS and 2SLS models
#' Reject H0 if p<0.05

coef_diff <- coef(ivreg) - coef(ols)
vcov_diff <- vcov(ivreg) - vcov(ols)
x2_diff <- as.vector(t(coef_diff) %*% solve(vcov_diff) %*% coef_diff)
pchisq(x2_diff, df=2, lower.tail=FALSE)

#' Finding: There is endogeneity in the data; hence our 2SLS model is better.

#' Checking for simultaneity (using 2SLS and 3SLS)
#' Does healthins reduce medexpense or does medexpense reduce healthins?
#' We must test two alternate equations, using a "simultaneous equation model"
#' Requires systemfit() function from SYSTEMFIT package.

install.packages("systemfit")
library(systemfit)

X2 <- cbind(illnesses)                      # Exogeneous variables for second equation
IV2 <- cbind(firmsize)                      # Instrumental variable for second equation
eq1 <- Y1 ~ Y2 + X1 + IV
eq2 <- Y2 ~ Y1 + X2 + IV2
inst <- ~ X1 + IV + IV2                     # For the system of equation, instrument includes all 
                                            # exogeneous variables and IV from both equations
system <- list(eq1=eq1, eq2=eq2)            # Specify system of equations - model that 2 equations

# 2SLS estimation
sls2 <- systemfit(system, "2SLS", inst=inst, data=d)
summary(sls2)
# 

# 3SLS estimation
sls3 <- systemfit(system, "3SLS", inst=inst, data=d)
summary(sls3)
