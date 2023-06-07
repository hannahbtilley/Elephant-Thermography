#------------------------------------------
# Physical activity and temperature changes of captive Asian elephants participating in eco-tourism activities   
#------------------------------------------
# Hannah B. TILLEY, Tsz Ching WONG, Derek MURPHY, Kaja WIERUCKA, Annaëlle SURREAULT-CHABLÊ, Hannah S. MUMBY

library(readr)
library(dplyr)
library(lme4)
library(e1071)
library(ggplot2)
library(car)
library(DHARMa)
library(patchwork)
library(emmeans)
library(lmerTest)
library(ggpubr)
library(merTools)
library(MASS)
library(tidyverse)
library(rstatix)

setwd("~/Desktop/Thermography Elephants/1. Shirley")

data <- read.csv("Master datasheet 09092022.csv", na = "NA") # data for all 6 elephant polo days (half and full day)
str(data)

data$Date <- as.Date(data$Date, format = "%d-%m-%y")
data$Elephant_individual <- as.factor(data$Elephant_individual)
data$PoloPractice <- as.factor(data$PoloPractice)
data$Before.After <- as.factor(data$Before.After)
data$Body_part <- as.factor(data$Body_part)
data$ID <- paste(data$Date, data$Elephant_individual)


# Setting the theme for plots
mynamestheme <- theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = (13)), 
                      legend.title = element_text(colour = "black",  face = "bold.italic", family = "Times New Roman", size = (13)), 
                      legend.text = element_text(face = "italic", colour="black",family = "Times New Roman", size = (13)), 
                      axis.title = element_text(family = "Times New Roman", face= "bold", size = (15), colour = "black"),
                      axis.text = element_text(family = "Times New Roman", colour = "black", size = (15)))

# MODEl------------------------------------------
model_all <- data %>% dplyr::select(ID, Date, Before.After, PoloPractice, Body_part, Temperature, Elephant_individual) %>% group_by(ID,Body_part, .drop=FALSE) %>%
  arrange(Before.After,.by_group = TRUE) %>% mutate(Temperature_difference = Temperature[1]-Temperature[2]) %>% ungroup() %>% filter(Before.After == "Before")

mod <- lmer(Temperature_difference ~ PoloPractice * Body_part + (1|Elephant_individual) + (1|Date), data= model_all)
summary(mod)
anova(mod) # likelihood ratio test

# Final LMM model
mod1 <- lmer(Temperature_difference ~ PoloPractice + Body_part + (1|Date) + (1|Elephant_individual), data= model_all)
summary(mod1)
anova(mod1)

# check model fit
check_model <- simulateResiduals(fittedModel = mod1, n = 500)
par(mar = c(2, 2, 2, 2))
plot(check_model)
plot(effects::allEffects(mod1))  
qqnorm(resid(mod1))
qqline(resid(mod1))

plot(predict(mod1))


# PLOTS------------------------------------------
# Figure 2: Temperature difference before and after polo
annot_1 <- "*"
annot_2 <- "**"
annot_3 <- "***"

pred <- cbind(model_all, predictInterval(mod1, data))
newdat <- with(model_all, expand.grid(Temperature_difference = unique(Temperature_difference), Body_part = unique(Body_part), Date = unique(Date), PoloPractice = unique(PoloPractice), Elephant_individual = unique(Elephant_individual)))
pred <- cbind(newdat, predictInterval(mod1, newdat))

pred %>% ggplot(aes(x= Body_part, y= fit)) + geom_boxplot(alpha =0.4, fill= "grey20", lwd= 0.7, notch = TRUE) + xlab("Body Region") + ylab("Temperature Difference (°C)") + theme_bw() + mynamestheme +
  scale_x_discrete(labels = c("Average", "Axilla", "Foreleg","Pinna","Shoulder")) + ylim(4,30) + geom_signif(annotations = c(formatC(annot_2, digits=2),formatC(annot_3, digits= 2), formatC(annot_1, digits= 2)), 
                                                                                                             y_position = c(23,25,27), xmin=c(1,1,1), xmax=c(2,4,5), size = 0.5, textsize=10, family = "serif")

# Figure 3: Temperature before and after polo- polo playing elephants
Fig3 <- data %>% dplyr::select(ID, Before.After, Body_part, Temperature, Elephant_individual) %>% group_by(ID,Body_part,.drop=FALSE)

Fig3 %>% ggplot(aes(x= Body_part, y=Temperature, fill= Before.After)) + xlab("Body Region") + 
  ylab("Temperature (°C)") + theme_bw() + mynamestheme + stat_boxplot(geom ='errorbar', width = 0.9) + ylim(0,40) +
  geom_boxplot(outlier.color = "black", outlier.shape = 21, outlier.fill = "white", outlier.size = 3, width = 0.9) + 
  scale_fill_manual(values=c("#DC143C","#add8e6")) + labs(fill = "Activity") + scale_x_discrete(labels = c("Average", "Axilla", "Foreleg","Pinna","Shoulder")) 

# ANOVA------------------------------------------
# comparison between half and full day of polo/ non- polo
full.halfdata <- read_csv("~/Desktop/Full_Half.csv")

model <-aov(Temperature_after ~ full.half + Body_part, data = full.halfdata)
summary(model)
TukeyHSD(model)

boxplot(full.halfdata$Temperature_after~full.halfdata$full.half)
boxplot(full.halfdata$Temperature_after~full.halfdata$PoloPractice)

