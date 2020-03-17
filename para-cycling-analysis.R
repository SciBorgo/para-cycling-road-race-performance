

#### Para-cycling analysis
install.packages(c("dplyr","lmerTest","emmeans","effsize","BayesFactor"))
library(dplyr)
library(lmerTest)
library(emmeans)
library(effsize)
library(BayesFactor)

#### Bicycling model
d = read.csv("para-cycling-data.csv")
d <- filter(d, Discipline =="C")

d$Class=as.factor(d$Class)
d$Gender=as.factor(d$Gender)
# Standardise age and distance
d$Age_s <- scale(d$Age, center = T, scale = T)

# Fit with age_s
fit <- lmer(Velocity ~ Event_order + Age_s + Gender + Class + Gender*Class + (1|Participant), data = d)
print(summary(fit),3)
confint(fit)
hist((residuals(fit)))
qqnorm(residuals(fit))

# Remove age_s term
fit <- lmer(Velocity ~ Event_order + Gender + Class + Gender*Class + (1|Participant), data = d)
print(summary(fit),3)
confint(fit)
hist((residuals(fit)))
qqnorm(residuals(fit))

# Constrasts
final_model <- emmeans(fit, list(pairwise ~ Class|Gender))
print(summary(final_model),3) # p-values
confint(final_model)

## Cohen's d
# Male classes
d1 <- filter(d, Gender =="1")
cohen.d(d1$Velocity[d1$Class=="C2"],d1$Velocity[d1$Class=="C1"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)
cohen.d(d1$Velocity[d1$Class=="C3"],d1$Velocity[d1$Class=="C2"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)
cohen.d(d1$Velocity[d1$Class=="C4"],d1$Velocity[d1$Class=="C3"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)
cohen.d(d1$Velocity[d1$Class=="C5"],d1$Velocity[d1$Class=="C4"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)
# Female classes
d2 <- filter(d, Gender =="2")
cohen.d(d2$Velocity[d2$Class=="C2"],d2$Velocity[d2$Class=="C1"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)
cohen.d(d2$Velocity[d2$Class=="C3"],d2$Velocity[d2$Class=="C2"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)
cohen.d(d2$Velocity[d2$Class=="C4"],d2$Velocity[d2$Class=="C3"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)
cohen.d(d2$Velocity[d2$Class=="C5"],d2$Velocity[d2$Class=="C4"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)

## Bayes factors
# Male classes
ttestBF(d1$Velocity[d1$Class=="C1"],d1$Velocity[d1$Class=="C2"], paired = F, rscale = "1")
ttestBF(d1$Velocity[d1$Class=="C2"],d1$Velocity[d1$Class=="C3"], paired = F, rscale = "1")
ttestBF(d1$Velocity[d1$Class=="C3"],d1$Velocity[d1$Class=="C4"], paired = F, rscale = "1")
ttestBF(d1$Velocity[d1$Class=="C4"],d1$Velocity[d1$Class=="C5"], paired = F, rscale = "1")
# Female classes
ttestBF(d2$Velocity[d2$Class=="C1"],d2$Velocity[d2$Class=="C2"], paired = F, rscale = "1")
ttestBF(d2$Velocity[d2$Class=="C2"],d2$Velocity[d2$Class=="C3"], paired = F, rscale = "1")
ttestBF(d2$Velocity[d2$Class=="C3"],d2$Velocity[d2$Class=="C4"], paired = F, rscale = "1")
ttestBF(d2$Velocity[d2$Class=="C4"],d2$Velocity[d2$Class=="C5"], paired = F, rscale = "1")


#### Tricycling model
d = read.csv("para-cycling-data.csv")
d <- filter(d, Discipline =="T")

d$Class=as.factor(d$Class)
d$Gender=as.factor(d$Gender)
# Standardise age and distance
d$Age_s <- scale(d$Age, center = T, scale = T)

# Fit with age_s
fit <- lmer(Velocity ~ Event_order + Age_s + Gender + Class + Gender*Class + (1|Participant), data = d)
print(summary(fit),3)
confint(fit)
hist((residuals(fit)))
qqnorm(residuals(fit))

# Remove age_s term
fit <- lmer(Velocity ~ Event_order + Gender + Class + Gender*Class + (1|Participant), data = d)
print(summary(fit),3)
confint(fit)
hist((residuals(fit)))
qqnorm(residuals(fit))

# Contrasts
final_model <- emmeans(fit, list(pairwise ~ Class|Gender))
print(summary(final_model),3) # p-values
confint(final_model)

## Cohen's d
# Male classes
d1 <- filter(d, Gender =="1")
cohen.d(d1$Velocity[d1$Class=="T2"],d1$Velocity[d1$Class=="T1"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)
# Female classes
d2 <- filter(d, Gender =="2")
cohen.d(d2$Velocity[d1$Class=="T2"],d2$Velocity[d1$Class=="T1"], paired = F, noncentral = F, na.rm = T, conf.level=0.90)

## Bayes factors
# Male classes
ttestBF(d1$Velocity[d1$Class=="T1"],d1$Velocity[d1$Class=="T2"], paired = F, rscale = "1")
# Female classes
ttestBF(d2$Velocity[d2$Class=="T1"],d2$Velocity[d2$Class=="T2"], paired = F, rscale = "1")

