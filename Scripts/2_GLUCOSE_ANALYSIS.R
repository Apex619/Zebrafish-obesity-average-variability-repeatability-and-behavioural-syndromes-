#Import data
Glucose_Weight <- read.csv("Data/Glucose/Glucose_Weight.csv")

#Check structure
Glucose_Weight$Sex <- as.factor(Glucose_Weight$Sex)
Glucose_Weight$Group <- as.factor(Glucose_Weight$Group)
Glucose_Weight$Meter <- as.factor(Glucose_Weight$Meter)

hist(Glucose_Weight$Glucose)
#Very high reading confirmed to be correct



Boxplot_Glucose <- ggplot(data = Glucose_Weight) +
  geom_boxplot(mapping = aes(x = Sex, y = Glucose, fill = Group)) +
  facet_grid(~Group)

Boxplot_Glucose

Summary_Glucose <- summarySE(Glucose_Weight, measurevar="Glucose", groupvars=c("Group", "Sex"))

Repeatability_Glucose <- rpt(Glucose ~ Meter + Sex + Group + (1 | Fish_ID), grname = "Fish_ID", data = Glucose_Weight, datatype = "Gaussian", 
                           nboot = 100, npermut = 100)

Repeatability_Glucose

Mixed_Model_Glucose <- lmer(Glucose ~ Sex + Group + (1|Fish_ID), data=Glucose_Weight)
summary(Mixed_Model_Glucose)
tab_model(Mixed_Model_Glucose)
