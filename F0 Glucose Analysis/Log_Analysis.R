#Log transformed analysis on glucose data

library(ggplot2)
library(lme4)
library(lmerTest)
library(sjPlot)
library(tidyverse)
library(effects)
library(Rmisc)
library(easyGgplot2)
library(rptR)

library(readr)
Glucose_Weight <- read_csv("Glucose_Weight.csv")

sqrtGlucose <- sqrt(Glucose_Weight$Glucose)
hist(sqrtGlucose)


log10Glucose <- log10(Glucose_Weight$Glucose)
hist(log10Glucose)

logGlucose <- log(Glucose_Weight$Glucose)
hist(logGlucose)

Glucose_Weight$log_Glucose <- log(Glucose_Weight$Glucose)

hist(Glucose_Weight$Weight)
log <- log(Glucose_Weight$Weight)
hist(log)
Glucose_Weight$log_Weight <- log(Glucose_Weight$Weight)

Glucose_Weight$Sex <- as.factor(Glucose_Weight$Sex)
Glucose_Weight$Group <- as.factor(Glucose_Weight$Group)
Glucose_Weight$Meter <- as.factor(Glucose_Weight$Meter)

hist(Glucose_Weight$Glucose)
#Very high reading confirmed to be correct

Scatterplot <- ggplot(Glucose_Weight, aes(x=log_Weight, y=log_Glucose)) + 
  geom_point()+
  geom_smooth(method=lm)

Scatterplot

Boxplot <- ggplot(data = Glucose_Weight) +
  geom_boxplot(mapping = aes(x = Sex, y = log_Glucose, fill = Group)) +
  facet_grid(~Group)

Boxplot

Summary <- summarySE(Glucose_Weight, measurevar="log_Glucose", groupvars=c("Group", "Sex"))
knitr::kable(
  Summary[1:4, ], 
  caption = "Summary"
)

Scatterplot_Function <- ggplot(Glucose_Weight, aes(x=Weight, y=log_Glucose, shape = Group, color = Group)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  facet_grid(~Sex)  
Scatterplot_Function

#Scatterplot_Jitter <- ggplot(log_Glucose_Weight, aes(log_Glucose, Weight, colour = Group)) +
#geom_jitter() +
#theme_bw() +
#facet_grid(~Sex) +
#stat_smooth(method = "lm", formula = y ~ x + I(x^2), aes(fill = Group))
#Scatterplot_Jitter

Repeatability_log_Glucose <- rpt(log_Glucose ~ Meter + Sex + Group + (1 | Fish_ID), grname = "Fish_ID", data = Glucose_Weight, datatype = "Gaussian", 
                             nboot = 0, npermut = 0)

Repeatability_log_Glucose

Mixed_Model_log_Glucose <- lmer(log_Glucose ~ Sex*Group + log_Weight + (1|Fish_ID), data=Glucose_Weight)
summary(Mixed_Model_log_Glucose)
tab_model(Mixed_Model_log_Glucose)

Mixed_Model_log_Weight <- lmer(log_Weight ~ Sex*Group + (1|Fish_ID), data=Glucose_Weight)
tab_model(Mixed_Model_log_Weight)

#rmarkdown::render("analysis.R")