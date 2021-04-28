

# Lapped analysis
# Author: DNB

# Libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(cowplot)

# Age description by gender and class
d = read.csv("para-cycling-data.csv") %>%
  clean_names() %>%
  select(event_year, participant, gender, class, age) %>%
  group_by(participant,event_year,gender,class) %>%
  mutate(age = as.numeric(age)) %>%
  summarise(median_age = median(age, na.rm = T))

d %>%
  group_by(event_year, class, gender) %>%
  summarise(mean_age = mean(median_age, na.rm = T),
            sd_age = sd(median_age, na.rm = T)) %>%
  mutate(
    sd_lower = mean_age-sd_age,
    sd_upper = mean_age+sd_age,
    gender = as.factor(gender)
  ) %>%
  ggplot(aes(x = event_year, y = mean_age, group = gender, colour = gender)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_line() +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper), width = 0, position = position_dodge(width = 0.2)) +
  theme_bw(base_size = 10) +
  facet_wrap(~class) +
  scale_colour_manual(values = c("gray65","gray20")) +
  scale_x_continuous(limits = c(2010.75, 2019.25), breaks = seq(2011, 2019, by = 1)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )
#ggsave(file = "para-age-by-class.png", units="in", width = 8, height = 6, dpi = 300)


# Age description by gender and discipline
d = read.csv("para-cycling-data.csv") %>%
  clean_names() %>%
  select(event_year, participant, gender, discipline, age) %>%
  group_by(participant, event_year, gender, discipline) %>%
  mutate(age = as.numeric(age)) %>%
  summarise(median_age = median(age, na.rm = T))

d %>%
  group_by(event_year, discipline, gender) %>%
  summarise(mean_age = mean(median_age, na.rm = T),
            sd_age = sd(median_age, na.rm = T)) %>%
  mutate(
    sd_lower = mean_age-sd_age,
    sd_upper = mean_age+sd_age,
    gender = as.factor(gender),
    gender = recode_factor(gender, '1' = 'Men', '2' = 'Women'),
    discipline = recode_factor(discipline, 'C' = '(A) Bicycling', 'T' = '(B) Tricycling')
  ) %>%
  ggplot(aes(x = event_year, y = mean_age, group = gender, colour = gender)) +
  geom_point(position = position_dodge(width = 0.25), size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper), width = 0.25, position = position_dodge(width = 0.2)) +
  theme_bw(base_size = 14) +
  facet_wrap(~discipline) +
  scale_colour_manual(values = c("gray65","gray20")) +
  scale_x_continuous(limits = c(2010.75, 2019.25), breaks = seq(2011, 2019, by = 1)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 14, hjust = 0)
  ) +
  labs(x = "Year", y = "Age (years)") + 
  guides(colour = guide_legend(title = ""))
#ggsave(file = "para-age-by-discipline.png", units="in", width = 8, height = 4, dpi = 300)
ggsave(file = "para-age-by-discipline.tiff", units="in", width = 8, height = 4, dpi = 300, compression = "lzw")











