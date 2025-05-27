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
  summarise(Toral_size = sum(size_MB))
grouped_data <-grouped_data[nchar(grouped_data$Platform) <= 30, ]
grouped_data$Sum_Size_TB <- round(as.numeric(grouped_data$Toral_size) / (1024*1024),0)



grouped_data$Platform <- factor(grouped_data$Platform, levels = unique(grouped_data$Platform[order(grouped_data$Sum_Size_TB)]))
grouped_data <- grouped_data %>%
  mutate(Sum_Size_TB_Percentage = (Sum_Size_TB / sum(Sum_Size_TB)) * 100)
grouped_data$Sum_Size_TB_Percentage <- round(grouped_data$Sum_Size_TB_Percentage, 1)
grouped_data$Sum_Size_TB_Percentage <- paste0("~", grouped_data$Sum_Size_TB_Percentage, "%")

ggplot(grouped_data, aes(x = Platform, y = Sum_Size_TB)) +
  geom_bar(stat = "identity",width=0.3, position = position_dodge(width = 0.6), alpha = 0.95, color = "black",fill= "#8da0cb") +
  geom_text(aes(label = Sum_Size_TB_Percentage), position = position_dodge(width = 1), hjust = -0.08, vjust = 0.4, size = 5) +
  coord_flip() +
  labs(x = "Technology", y = "Size (TB)", title = "") +
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
    breaks = c(0,23900),
    labels = c("0","23900"),
    limits=c(0,27000),
    expand = expansion(add = c(10,10))  
    
  ) 

