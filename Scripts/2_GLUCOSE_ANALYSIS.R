#wd
#setwd("C:/Users/USER/Dropbox/Zebrafish Experiment/Big Experiment/Glucose")

library(ggplot2)
library(lme4)
library(lmerTest)
library(sjPlot)
library(tidyverse)
library(effects)
library(Rmisc)
library(easyGgplot2)
library(rptR)

Glucose_Weight <- read_csv("Data/Glucose/Glucose_Weight.csv")

Glucose_Weight$Sex <- as.factor(Glucose_Weight$Sex)
Glucose_Weight$Group <- as.factor(Glucose_Weight$Group)
Glucose_Weight$Meter <- as.factor(Glucose_Weight$Meter)

hist(Glucose_Weight$Glucose)
#Very high reading confirmed to be correct

Scatterplot <- ggplot(Glucose_Weight, aes(x=Weight, y=Glucose)) + 
  geom_point()+
  geom_smooth(method=lm)

Scatterplot

Boxplot <- ggplot(data = Glucose_Weight) +
  geom_boxplot(mapping = aes(x = Sex, y = Glucose, fill = Group)) +
  facet_grid(~Group)

Boxplot

Summary <- summarySE(Glucose_Weight, measurevar="Glucose", groupvars=c("Group", "Sex"))
knitr::kable(
  Summary[1:4, ], 
  caption = "Summary"
)

Scatterplot_Function <- ggplot(Glucose_Weight, aes(x=Weight, y=Glucose, shape = Group, color = Group)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  facet_grid(~Sex)  
Scatterplot_Function

#Scatterplot_Jitter <- ggplot(Glucose_Weight, aes(Glucose, Weight, colour = Group)) +
  #geom_jitter() +
  #theme_bw() +
  #facet_grid(~Sex) +
  #stat_smooth(method = "lm", formula = y ~ x + I(x^2), aes(fill = Group))
#Scatterplot_Jitter

Repeatability_Glucose <- rpt(Glucose ~ Meter + Sex + Group + (1 | Fish_ID), grname = "Fish_ID", data = Glucose_Weight, datatype = "Gaussian", 
                           nboot = 0, npermut = 0)

Repeatability_Glucose

Mixed_Model_Glucose <- lmer(Glucose ~ Sex + Group + (1|Fish_ID), data=Glucose_Weight)
summary(Mixed_Model_Glucose)
tab_model(Mixed_Model_Glucose)

#rmarkdown::render("analysis.R")