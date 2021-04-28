

# Lapped analysis
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(cowplot)

# Load data
d = read.csv("para-dns-dnf-lapped.csv") %>%
  clean_names()

# Bicycling
c_classes <- d %>% filter(lapped == "1") %>%
  filter(class %in% c("C1","C2","C3","C4","C5")) %>%
  group_by(gender, class) %>%
  summarise(count = n()) %>%
  mutate(frequency = count / sum(count)) # Freq within each gender

c_classes %>% 
  mutate(
    gender = recode_factor(gender, 'M' = 'Men', 'W' = 'Women')
  ) %>%
  ggplot(aes(x = gender, y = frequency, fill = class)) +
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 14) +
  scale_fill_brewer() +
  ggtitle("Lapped Bicycling Athletes") +
  labs(x = "", y = "Proportion") +
  guides(fill = guide_legend(title = "Class")) -> plot_c
plot_c

# Tricycling
t_classes <- d %>% filter(lapped == "1") %>%
  filter(class %in% c("T1","T2")) %>%
  group_by(gender, class) %>%
  summarise(count = n()) %>%
  mutate(frequency = count / sum(count)) # Freq within each gender

t_classes %>% 
  mutate(
    gender = recode_factor(gender, 'M' = 'Men', 'W' = 'Women')
  ) %>%
  ggplot(aes(x = gender, y = frequency, fill = class)) +
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 14) +
  scale_fill_brewer() +
  ggtitle("Lapped Tricycling Athletes") +
  labs(x = "", y = "Proportion") +
  guides(fill = guide_legend(title = "Class")) -> plot_t
plot_t

# Panel plot
plot_grid(plot_c, plot_t, ncol = 2, nrow = 1, labels = NULL, label_size = 16, align = 'vh', axis = "lr")
ggsave(file = "para-lapped-analysis.png", units="in", width = 9, height = 4, dpi = 300)










