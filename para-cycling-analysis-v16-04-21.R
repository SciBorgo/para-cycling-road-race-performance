
# Para-cycling analysis
# Author: David N Borg
# Date: April 2021

# Libraries
library(dplyr)
library(lmerTest)
library(emmeans)
library(effsize)
library(ggplot2)
library(visdat)
library(naniar)
library(mice)
library(broom.mixed)
library(janitor)


#### Bicycling model
d = read.csv("para-cycling-data.csv") %>% 
  filter(Discipline =="C") %>%
  mutate(
    Class = as.factor(Class),
    Gender = as.factor(Gender),
    Age_s = scale(Age, center = T, scale = T),
    Age_s = as.numeric(Age_s)
  ) %>%
  clean_names()

# Explore missing data age
vis_miss(d)
visdat::vis_dat(d)

ggplot(d,
       aes(x = age,
           y = velocity)) +
  geom_miss_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", colour = "black") +
  theme_bw() +
  facet_grid(~gender)

gg_miss_fct(x = d, fct = gender)

# Multiple imputation
sub <- d %>%
  group_by(participant) %>%
  select(participant, age_s, event_order, class, gender, velocity)

sapply(sub, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

imp_datasets <- mice(sub, m = 5, method = "rf", seed = 123)
stripplot(imp_datasets, age_s, pch = 19, xlab = "Imputation number")

# Fit the imputed datasets
fit <- with(data = imp_datasets, exp = lmer(velocity ~ (1|event_order) + age_s + gender + class + gender*class + (1|participant)))
print(summary(pool(fit), conf.int = T), digits = 2)

# Remove age and remodel
fit <- lmer(velocity ~ (1|event_order) + gender + class + gender*class + (1|participant), data = sub)

# Summary
summary(fit)

# Confidence intervals
confint(fit)

# Diagnositcs
hist((residuals(fit)))
qqnorm(residuals(fit)); qqline(residuals(fit))

# Look more closely at residuals
qqnorm(residuals(fit)); qqline(residuals(fit))
rs <- residuals(fit) 
rs_q <- quantile(rs, probs = c(0.05,0.95))
pot_out <- rs < rs_q[1] | rs > rs_q[2]
d_outlier <- as.data.frame(d)[pot_out,] %>%
  dplyr::select(participant, event_order, class, gender, velocity)

d_outlier

d_no_out <- d %>% anti_join(d_outlier, by = c("participant","event_order","gender","class")) # keep rows without matching ID

fit_refit <- lmer(velocity ~ (1|event_order) + gender + class + gender*class + (1|participant), 
             data = d_no_out)

qqnorm(residuals(fit_refit)); qqline(residuals(fit_refit))

# Plot the outliers removed
ggplot(data = d_outlier, aes(x = class, y = velocity)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  facet_grid(~gender) +
  theme_bw()

fit = fit_refit

# Constrasts
final_model <- emmeans(fit, list(pairwise ~ class|gender))
print(summary(final_model),3) # p-values
confint(final_model)

## Cohen's d
# Male classes
d1 <- filter(d, gender == "1")
cohen.d(d1$velocity[d1$class == "C2"], d1$velocity[d1$class == "C1"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)
cohen.d(d1$velocity[d1$class == "C3"], d1$velocity[d1$class == "C2"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)
cohen.d(d1$velocity[d1$class == "C4"], d1$velocity[d1$class == "C3"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)
cohen.d(d1$velocity[d1$class == "C5"], d1$velocity[d1$class == "C4"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)
# Female classes
d2 <- filter(d, gender == "2")
cohen.d(d2$velocity[d2$class == "C2"], d2$velocity[d2$class == "C1"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)
cohen.d(d2$velocity[d2$class == "C3"], d2$velocity[d2$class == "C2"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)
cohen.d(d2$velocity[d2$class == "C4"], d2$velocity[d2$class == "C3"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)
cohen.d(d2$velocity[d2$class == "C5"], d2$velocity[d2$class == "C4"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)








#### Tricycling model
d = read.csv("para-cycling-data.csv") %>%
  filter(Discipline == "T") %>%
  mutate(
    Class = as.factor(Class),
    Gender = as.factor(Gender),
    Age_s = scale(Age, center = T, scale = T),
    Age_s = as.numeric(Age_s)
  ) %>%
  clean_names()

# Multiple imputation
sub <- d %>%
  group_by(participant) %>%
  select(participant, age_s, event_order, class, gender, velocity)

sapply(sub, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

imp_datasets <- mice(sub, m = 5, method = "rf", seed = 123)
stripplot(imp_datasets, age_s, pch = 19, xlab = "Imputation number")

# Fit the imputed datasets
fit <- with(data = imp_datasets, exp = lmer(velocity ~ (1|event_order) + age_s + gender + class + gender*class + (1|participant)))
print(summary(pool(fit), conf.int = T), digits = 2)

# Remove age and remodel
fit <- lmer(velocity ~ (1|event_order) + gender + class + gender*class + (1|participant), data = d)

# Summary
summary(fit)

# Confidence intervals
confint(fit)

# Diagnositcs
hist((residuals(fit)))
qqnorm(residuals(fit)); qqline(residuals(fit))

# Look more closely at residuals
qqnorm(residuals(fit)); qqline(residuals(fit))
rs <- residuals(fit) 
rs_q <- quantile(rs, probs = c(0.05,0.95))
pot_out <- rs < rs_q[1] | rs > rs_q[2]
d_outlier <- as.data.frame(d)[pot_out,] %>%
  dplyr::select(participant, event_order, class, gender, velocity)

d_outlier

d_no_out <- d %>% anti_join(d_outlier, by = c("participant","event_order","gender","class")) # keep rows without matching ID

fit_refit <- lmer(velocity ~ (1|event_order) + gender + class + gender*class + (1|participant), 
                  data = d_no_out)

qqnorm(residuals(fit_refit)); qqline(residuals(fit_refit))

# Plot of the outliers that were removed
ggplot(data = d_outlier, aes(x = class, y = velocity)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  facet_grid(~gender) +
  theme_bw()

fit = fit_refit

# Contrasts
final_model <- emmeans(fit, list(pairwise ~ class|gender))
print(summary(final_model),3) # p-values
confint(final_model)

## Cohen's d
# Male classes
d1 <- filter(d, gender == "1")
cohen.d(d1$velocity[d1$class == "T2"], d1$velocity[d1$class == "T1"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)
# Female classes
d2 <- filter(d, gender == "2")
cohen.d(d2$velocity[d1$class == "T2"], d2$velocity[d1$class == "T1"], paired = F, noncentral = F, na.rm = T, conf.level = 0.90)

