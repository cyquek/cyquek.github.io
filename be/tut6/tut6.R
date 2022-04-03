# ECOM30001/ECOM90001 Basic Econometrics, Semester 1, 2022
# Tutorial 6
# Andy Clarke
rm(list=ls())                          # clear memory
#----------------------------------------
setwd("E:/teaching/basic econometrics/Semester 1 2022/tutorials/tutorial 6")
#---------------------------------------
options(scipen=999)
library(ggplot2)
library(car)
library(lmtest)
library(stargazer)
library(sandwich)
library(scales)        # package to display thousands with commas in graphs
#-------------------------------
# Question 2: Health Expenditures
#------------------------------
# gdp:        gross domestic product (current $US)
# health    : current health expenditures (current $US)
# Read raw data into R
tut6 <- read.csv("tut6.csv")
# generate health pop ratio
tut6$healthpop <- tut6$health/tut6$pop
# generate gdp pop ratio
tut6$gdppop <- tut6$gdp/tut6$pop
mean_gdppop <-mean(tut6$gdppop)
print(mean_gdppop)
# OLS  Model (Ignore heteroskedasticity)
# dependent variable: healthpop
# explanatory variable: gdppop
reg3 <- lm(healthpop ~ gdppop, data=tut6)
print(summary(reg3))
# Sample F statistic
wald_ols <- waldtest(reg3)                            # sample F statistics of overall significance
print(wald_ols)                                       # print sample F statistic  
RSS_reg3 <- deviance(reg3)                            # residual sum of squares for reg3
resid_reg3 <- reg3$residuals                          # generate series for residuals for reg3                    
yhat_reg3 <- reg3$fitted.values                       # generate series for fitted values for reg3
demdf1 <- df.residual(reg3)                           # denominator df
fstat1 <- round(wald_ols$"F"[2], digits=4)            # Sample value of F stat
pvalf1 <- round(wald_ols$'Pr(>F)'[2], digits=4)       # p value of F test
numdf1 <- abs(wald_ols$"Df"[2])                       # numerator df 
print(fstat1)
print(pvalf1)
print(numdf1)
#----------------------------------------
# Two-sided test H0: beta1 = 0 HA: beta1 ne 0
#------------------------------------------
alpha <- 0.05                                     # set level of significance
b1_reg3 <- coef(reg3)[["gdppop"]]                 # coefficient on gdppop
seb1_reg3 <- sqrt(vcov(reg3)[2,2])                # standard error of b1
df_reg3 <- df.residual(reg3)                      # degrees of freedom
t_3 <- (b1_reg3-0)/seb1_reg3                      # construct t test statistic
tcr_3 <- qt(1-alpha/2, df_reg3)                   # calculate critical value
pval_3 <- 2*(1-pt(abs(t_3), df_reg3))             # calculate p-value for 2 sided test
print(b1_reg3)
print(seb1_reg3)
print(df_reg3)                                    # print degrees of freedom 
print(t_3)                                        # print sample t test statistic
print(tcr_3)                                      # print t critical value
print(pval_3)                                     # print p value for sample t statistic
#----------------------------------
# Plot the actual and fitted values
#----------------------------------
ggplot(tut6, aes(x=gdppop)) +
  geom_point(aes(y=healthpop, colour="Actual Data")) +
  geom_line(aes(y=yhat_reg3, colour="Fitted Values: Linear Model"), size=1) +  
  labs(x = "Gross Domestic Product, per capita", y = "Health Expenditures, per capita") +
  scale_y_continuous(breaks = round(seq(0,10000, by = 2000),1),labels=comma) +
  scale_x_continuous(breaks = round(seq(0,90000, by = 10000),1),labels=comma) +
                         scale_colour_manual("", 
                      breaks = c("Actual Data", "Fitted Values: Linear Model"),
                      values = c("blue", "red")) +
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10)) 
 ggsave("tut6_graph1.pdf")
#---------------------------- 
# (2) Plot the OLS Residuals
#---------------------------- 
 ggplot(tut6, aes(x=gdppop)) +
   geom_bar(aes(y=resid_reg3), colour="dark green",width=0.5, fill="dark green", 
            stat="identity") + 
   geom_hline(yintercept=0, color="black") + 
   geom_vline(xintercept=mean_gdppop, color="black") +
   labs(x = "GDP, per capita", y = "OLS Residuals: Linear Model") +
   scale_y_continuous(breaks = round(seq(-4000,4000, by = 1000),1), labels=comma) +
   scale_x_continuous(breaks = round(seq(0,900000, by = 10000),1), labels=comma) +
   theme_classic() +
   theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
    ggsave("tut6_graph2.pdf")
 #-----------------------------
# Robust (Huber-White) Standard Errors
 #---------------------------
# Use sandwich package
cov_reg3 <- vcovHC(reg3, type = "HC1")
print(cov_reg3)                               # print robust variance=covariance matrix
robust_se_reg3    <- sqrt(diag(cov_reg3))     # robust std errors as sq root of main diagonals      
# Adjusted F statistic (Robust standard errors)
wald_r <- waldtest(reg3, vcov = cov_reg3)     # sample F statistic using the robust variance-cov matrix
print(wald_r)               
  
fstat2 <- round(wald_r$"F"[2], digits=4)           # Sample value of F stat based on robust var-cov matrix
pvalf2 <- round(wald_r$'Pr(>F)'[2], digits=4)      # p value of F test
numdf2 <- abs(wald_r$"Df"[2])                      # numerator df 
print(fstat2)
print(pvalf2)
print(numdf2)
#----------------
# Two-sided test H0: beta1 = 0 HA: beta1 ne 0
# Use robust standard errors
#------------------------------
coeftest(reg3,vcov=cov_reg3)
b1_reg3 <- coef(reg3)[["gdppop"]]              # coefficient on gdppop
print(b1_reg3)
seb1_reg3_r <- sqrt(cov_reg3[2,2])             # robust standard error of b1
print(seb1_reg3_r)
df_reg3 <- df.residual(reg3)                   # degrees of freedom
t_4 <- (b1_reg3-0)/seb1_reg3_r                 # construct t test statistic
tcr_4 <- qt(1-alpha/2, df_reg3)                # calculate critical value
pval_4 <- 2*(1-pt(abs(t_4), df_reg3))          # calculate p-value for 2 sided test
print(df_reg3)                                 # print degrees of freedom
print(t_4)                                     # print robust sample t test statistic
print(tcr_4)                                   # print t critical value
print(pval_4)                                  # print robust p value for sample t stat
#---------------------------
# Generalised Least Squares (GLS)
# Heteroskedasticity: var = sigma2*gdppop
tut6$weight1 <- 1/tut6$gdppop                    # weight for GLS
reg3_gls <- lm(healthpop~ gdppop, weight=weight1, data=tut6)
print(summary(reg3_gls))                      # print GLS results
# Sample F statistic
wald_gls <-waldtest(reg3_gls)                 # sample F statistic for GLS model
print(wald_gls)                               # print sample F statistic
fstat3 <- round(wald_gls$"F"[2], digits=4)           # Sample value of F stat for GLS model
pvalf3 <- round(wald_gls$'Pr(>F)'[2], digits=4)      # p value of F test
numdf3 <- abs(wald_gls$"Df"[2])                      # numerator df 
print(fstat3)
print(pvalf3)
print(numdf3)
#--------------------------------
# Two-sided test H0: beta1 = 0 HA: beta1 ne 0
# Based don GLS reuslts
#--------------------------------
coeftest(reg3_gls)
b1_reg3_gls <- coef(reg3_gls)[["gdppop"]]     # GLS coefficient on gdppop
print(b1_reg3_gls)
seb1_reg3_gls <- sqrt(vcov(reg3_gls)[2,2])    # GLS standard error of b1
print(seb1_reg3_gls)
df_reg3_gls <- df.residual(reg3_gls)          # degrees of freedom
t_5 <- (b1_reg3_gls-0)/seb1_reg3_gls          # construct t test statistic
tcr_5 <- qt(1-alpha/2, df_reg3_gls)           # calculate critical value
pval_5 <- 2*(1-pt(abs(t_5), df_reg3_gls))     # calculate p-value for 2 sided test
print(df_reg3_gls)                            # print degrees of freedom for GLS model
print(t_5)                                    # print sample t test statistic for GLS model
print(tcr_5)                                  # print t critical value
print(pval_5)                                 # print p value for sample t statistic GLS model
#--------------------------------------------------
# Note: denominator df for F stat same for OLs,robust, and GLS models so use demf1
# stargazer output: compare the models
# Summarize the three models using stargazer
stargazer(reg3, reg3, reg3_gls, type = "html", 
          dep.var.labels=c("Health Expenditure, per capita"),
          covariate.labels=c("Intercept", "GDP, per captia"),
          column.labels = c("OLS", "Robust (White)", "GLS"),
          se        = list(NULL, robust_se_reg3, NULL),
          omit.stat = "f",
          add.lines = list(c("F Statistic", fstat1, fstat2, fstat3),
                           c("F p value", pvalf1, pvalf2, pvalf3),
                           c("F num df", numdf1, numdf2, numdf3),
                           c("F dem df", demdf1, demdf1, demdf1)),
          digits=4, align=TRUE,
          intercept.bottom=FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out= "tut6_reg2.html")