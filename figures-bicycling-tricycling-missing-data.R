
# Para-cycling analysis
# Author: David N Borg
# Date: April 2021


# Libraries
library(dplyr)
library(ggplot2)
library(visdat)
library(naniar)
library(janitor)
library(cowplot)


#### Figures
# Bicycling plot
d = read.csv("para-cycling-data.csv") %>% 
  filter(Discipline =="C") %>%
  mutate(
    Class=as.factor(Class),
    Gender_name=as.factor(Gender_name)
  )

d %>% filter(Gender_name == "Men") %>%
  mutate(
    Gender_name = recode_factor(Gender_name, 'Men' = '(A) Men')
  ) %>%
  ggplot(aes(x = as.factor(Class), y = Velocity)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.175, alpha = 0.065, size = 2) +
  theme_bw() + 
  ylab(Race~Velocity~(km~h^{-1})) +
  xlab("Bicycling Class") +
  scale_y_continuous(limits = c(25,50)) +
  theme(axis.text=element_text(size = 12),
        axis.title=element_text(size = 14),
        legend.text=element_text(size = 12),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_text(size = 14, hjust = 0)) +
  #ggtitle("(A) Men") +
  geom_segment(aes(x=1,xend=2,y=43,yend=43), colour = "black",size=.25) +
  geom_segment(aes(x=2,xend=3,y=45,yend=45), colour = "black",size=.25) +
  geom_segment(aes(x=3,xend=4,y=47,yend=47), colour = "black",size=.25) +
  geom_segment(aes(x=4,xend=5,y=49,yend=49), colour = "black",size=.25) +
  annotate(geom="text", x=1.5, y=43.5, label="*",size=5) +
  annotate(geom="text", x=2.5, y=45.5, label="*",size=5) +
  annotate(geom="text", x=3.5, y=47.5, label="*",size=5) +
  annotate(geom="text", x=4.5, y=49.5, label="*",size=5) +
  facet_grid(~Gender_name) -> plot1
plot1
#ggsave(file = "c-men.png", units="in", width = 5, height = 4, dpi = 300)

d %>% filter(Gender_name == "Women") %>%
  mutate(
    Gender_name = recode_factor(Gender_name, 'Women' = '(B) Women')
  ) %>%
  ggplot(aes(x = as.factor(Class), y = Velocity)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.175, alpha = 0.1, size = 2) +
  theme_bw() + 
  ylab(Race~Velocity~(km~h^{-1})) +
  xlab("Bicycling Class") +
  scale_y_continuous(limits = c(22.5,42.5)) +
  theme(axis.text=element_text(size = 12),
        axis.title=element_text(size = 14),
        legend.text=element_text(size = 12),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_text(size = 14, hjust = 0)) +
  #ggtitle("(B) Women") +
  geom_segment(aes(x=1,xend=2,y=38,yend=38), colour = "black",size=.25) +
  geom_segment(aes(x=3,xend=4,y=41,yend=41), colour = "black",size=.25) +
  annotate(geom="text", x=1.5, y=38.5, label="*",size=5) +
  annotate(geom="text", x=3.5, y=41.5, label="*",size=5) +
  facet_grid(~Gender_name) -> plot2
plot2
#ggsave(file = "c-women.png", units="in", width = 5, height = 4, dpi = 300)

# Bicycling panel
plot_grid(plot1, plot2, ncol = 1, nrow = 2) # labels = c('(A)','(B)'), label_size = 16
ggsave(file = "bicycling.tiff", units="in", width = 6, height = 7, dpi = 300, compression = "lzw")





# Tricycling plot
d = read.csv("para-cycling-data.csv") %>% 
  filter(Discipline =="T") %>%
  mutate(
    Class=as.factor(Class),
    Gender_name=as.factor(Gender_name)
  )

d %>% filter(Gender_name == "Men") %>%
  mutate(
    Gender_name = recode_factor(Gender_name, 'Men' = '(A) Men')
  ) %>%
  ggplot(aes(x = as.factor(Class), y = Velocity)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.175, alpha = 0.1, size = 3) +
  theme_bw() + 
  ylab(Race~Velocity~(km~h^{-1})) +
  xlab("Tricycling Class") +
  scale_y_continuous(limits = c(15,40), breaks = c(15,20,25,30,35,40)) +
  theme(axis.text=element_text(size = 12),
        axis.title=element_text(size = 14),
        legend.text=element_text(size = 12),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_text(size = 14, hjust = 0)) +
  #ggtitle("(A) Men") +
  geom_segment(aes(x=1,xend=2,y=38,yend=38), colour = "black",size=.25) +
  annotate(geom="text", x=1.5, y=39, label="*", size=5)+
  facet_grid(~Gender_name) -> plot1
plot1
#ggsave(file = "t-men.png", units="in", width = 5, height = 4, dpi = 300)

d %>% filter(Gender_name == "Women") %>%
  mutate(
    Gender_name = recode_factor(Gender_name, 'Women' = '(B) Women')
  ) %>%
  ggplot(aes(x = as.factor(Class), y = Velocity)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.175, alpha = 0.1, size = 3) +
  theme_bw() + 
  ylab(Race~Velocity~(km~h^{-1})) +
  xlab("Tricycling Class") +
  #scale_y_continuous(limits = c(15,40), breaks = c(15,20,25,30,35,40)) +
  theme(axis.text=element_text(size = 12),
        axis.title=element_text(size = 14),
        legend.text=element_text(size = 12),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_text(size = 14, hjust = 0)) +
  #ggtitle("(B) Women") +
  geom_segment(aes(x=1,xend=2,y=34.5,yend=34.5), colour = "black",size=.25) +
  annotate(geom="text", x=1.5, y=35.5, label="*", size=5) +
  facet_grid(~Gender_name) -> plot2
plot2
#ggsave(file = "t-women.png", units="in", width = 5, height = 4, dpi = 300)

# Tricycling panel
plot_grid(plot1, plot2, ncol = 2, nrow = 1) # labels = c('(A)','(B)'), label_size = 16
ggsave(file = "tricycling.tiff", units="in", width = 8, height = 4, dpi = 300, compression = "lzw")






# Missing data panel plot
d = read.csv("para-cycling-data.csv") %>%
  arrange(Participant) %>%
  select(Participant, Event, Distance, Age, Velocity, Gender, Class, Discipline)

vis_miss(d) -> plota
plota

d %>% mutate(
  Gender = as.factor(Gender),
  Gender = recode_factor(Gender, '1' = 'Male', '2' = 'Female'),
  Discipline = recode_factor(Discipline, 'C' = 'Bicycling', 'T' = 'Tricycling')
  ) %>% 
  select(Discipline, Age) %>% 
  gg_miss_fct(fct = Discipline) + labs(y = "") -> plotb
plotb


dsub <- d %>% mutate(
  Gender = as.factor(Gender),
  Gender = recode_factor(Gender, '1' = 'Male', '2' = 'Female'),
  Discipline = recode_factor(Discipline, 'C' = 'Bicycling', 'T' = 'Tricycling')
  )

dsub %>% ggplot(aes(x = Age,
           y = Velocity)) +
  geom_miss_point(size = 2, alpha = 0.25) +
  geom_smooth(method = "lm", colour = "black") +
  theme_bw() +
  ylab(Race~Velocity~(km~h^{-1})) +
  facet_grid(Gender~Discipline) -> plotc

# Panel
plot_grid(plota, plotb,
          labels = c('(A)','(B)'),
          label_size = 16) -> plot1
plot_grid(plotc,
          labels = c('(C)'),
          label_size = 16) -> plotc
plot_grid(plot1, plotc,
          ncol = 1, nrow = 2,
          scale = 0.9)
ggsave(file = "missing-data.tiff", units="in", width = 7, height = 7, dpi = 300, compression = "lzw")








