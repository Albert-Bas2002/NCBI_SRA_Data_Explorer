# Подключение библиотек
library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(patchwork)
library(gridExtra)

file <- "SRA_graf_inf.csv"

data <- read.csv(file)



grouped_data <- data %>%
  group_by(Platform) %>%
  summarise(Run = n())
grouped_data <-grouped_data[nchar(grouped_data$Platform) <= 30, ]


grouped_data$Platform <- factor(grouped_data$Platform, levels = unique(grouped_data$Platform[order(grouped_data$Run)]))
grouped_data <- grouped_data %>%
  mutate(Run_Percentage = (Run / sum(Run)) * 100)
grouped_data$Run_Percentage <- round(grouped_data$Run_Percentage, 1)
grouped_data$Run_Percentage <- paste0("~", grouped_data$Run_Percentage, "%")

ggplot(grouped_data, aes(x = Platform, y = Run)) +
  geom_bar(stat = "identity",width=0.3, position = position_dodge(width = 0.6), alpha = 0.95, color = "black",fill= "#8da0cb") +
  geom_text(aes(label = Run_Percentage), position = position_dodge(width = 1), hjust = -0.08, vjust = 0.4, size = 5) +
  coord_flip() +
  labs(x = "Technology", y = "Runs", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 15),   
    legend.title = element_text(size = 15), 
    
    panel.border = element_rect(color = "black", fill = NA, size = 0.7),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    legend.key.width = unit(0.50, "cm"),
    legend.key.height = unit(0.55, "cm"),
    legend.position = c(0.18, 0.28),
    
    legend.background = element_rect(color = "black", size = 0.2)
    
    
  )+
  scale_y_continuous(
    breaks = c(0,25000000),
    labels = c("0","25000000"),
    limits=c(0,27000000),
    expand = expansion(add = c(10,10))  
    
  ) 

