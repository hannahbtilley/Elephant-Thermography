#------------------------------------------
# Physical activity and temperature changes of Asian elephants (Elephas maximus) participating in eco-tourism activities and elephant polo
#------------------------------------------
# Hannah B. TILLEY, Derek MURPHY, Kaja WIERUCKA, Tsz Ching WONG, Annaëlle SURREAULT-CHÂBLE, Hannah S. MUMBY

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
library(performance)

# Comparison between half and full day of polo/ non-polo
read.csv("Full_Half.csv")

model <- lmer(Temperature_after ~ full.half + Body_part + (1|Elephant_individual), data = full.halfdata)
summary(model)

boxplot(full.halfdata$Temperature_after~full.halfdata$full.half)
boxplot(full.halfdata$Temperature_after~full.halfdata$PoloPractice)

# Data for all 6 elephant polo days (half and full day)
data <- read.csv("Master datasheet.csv", na = "NA") 
str(data)

data$Date <- as.Date(data$Date, format = "%d-%m-%y")
data$Elephant_individual <- as.factor(data$Elephant_individual)
data$PoloPractice <- as.factor(data$PoloPractice)
data$Before.After <- as.factor(data$Before.After)
data$Body_part <- as.factor(data$Body_part)
data$ID <- paste(data$Date, data$Elephant_individual)


# MODELS------------------------------------------
model_all <- data %>% dplyr::select(ID, Date, Before.After, PoloPractice, Body_part, Temperature, Elephant_individual) %>% group_by(ID,Body_part, .drop=FALSE) %>%
  arrange(Before.After,.by_group = TRUE) %>% mutate(Temperature_difference = Temperature[1]-Temperature[2]) %>% ungroup() %>% filter(Before.After == "Before")

mod <- lmer(Temperature_difference ~ PoloPractice * Body_part + (1|Elephant_individual) + (1|Date), data= model_all)
summary(mod)

# Likelihood ratio test
anova(mod) 

# Interaction term is not significant. In order to be able to interpret the results of the main effects, we must run a second model without the interaction term.
# Final LMM model
mod1 <- lmer(Temperature_difference ~ PoloPractice + Body_part + (1|Date) + (1|Elephant_individual), data= model_all)
summary(mod1)
anova(mod1)

summary(mod1)$varcor

# PLOTS------------------------------------------
# Figure 2: Temperature difference before and after polo
annot_1 <- "*"
annot_2 <- "**"
annot_3 <- "***"

pred <- cbind(model_all, predictInterval(mod1, data))
newdat <- with(model_all, expand.grid(Temperature_difference = unique(Temperature_difference), Body_part = unique(Body_part), Date = unique(Date), PoloPractice = unique(PoloPractice), Elephant_individual = unique(Elephant_individual)))
pred <- cbind(newdat, predictInterval(mod1, newdat))

pred %>% ggplot(aes(x= Body_part, y= fit)) + geom_boxplot(alpha =0.4, fill= "grey20", lwd= 0.7, notch = TRUE) + xlab("Body Region") + ylab("Temperature Difference (°C)") + theme_bw() +
  scale_x_discrete(labels = c("Average", "Axilla", "Foreleg","Pinna","Shoulder")) + ylim(4,30) + geom_signif(annotations = c(formatC(annot_2, digits=2),formatC(annot_3, digits= 2), formatC(annot_1, digits= 2)), 
                                                                                                             y_position = c(23,25,27), xmin=c(1,1,1), xmax=c(2,4,5), size = 0.5, textsize=10, family = "serif")
#######

# Plotting final graph - fitting new model with polo practice dummy coded and centered 
# (to plot effects for all elephants from both activity types- as polo practice was not significant) 
model_all$polo_dummy <- as.numeric(model_all$PoloPractice==levels(model_all$PoloPractice)[2])
model_all

# Creating dummy variable
model_all$polo_dummy <- model_all$polo_dummy-mean(model_all$polo_dummy)
mod1b <- lmer(Temperature_difference ~ polo_dummy + Body_part + (1|Date) + (1|Elephant_individual), data= model_all)
summary(mod1b)

pred <- cbind(model_all, predictInterval(mod1b, model_all))
newdat <- with(model_all, expand.grid(Temperature_difference = unique(Temperature_difference), Body_part = unique(Body_part), Date = unique(Date), polo_dummy = unique(polo_dummy), Elephant_individual = unique(Elephant_individual)))
pred <- cbind(newdat, predictInterval(mod1b, newdat))

# Final Graph
pred %>% ggplot(aes(x= Body_part, y= fit)) + geom_jitter(data =model_all[!is.na(model_all$Temperature_difference),], aes(x=Body_part, y=Temperature_difference), width = 0.2, alpha = 0.3) + geom_boxplot(alpha =0.4, fill= "grey20", lwd= 0.7, notch = TRUE) + xlab("Body Region") + ylab("Temperature Difference (°C)") + theme_bw() +
  scale_x_discrete(labels = c("Average", "Axilla", "Foreleg","Pinna","Shoulder")) + ylim(0,30) + geom_signif(annotations = c(formatC(annot_2, digits=2),formatC(annot_3, digits= 2), formatC(annot_1, digits= 2)), 
                                                                                                             y_position = c(22,25,28), xmin=c(1,1,1), xmax=c(2,4,5), size = 0.5, textsize=10, family = "serif")

