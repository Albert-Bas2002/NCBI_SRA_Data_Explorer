library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(tidyr)
library(knitr)
library(gt)

file <- "SRA_graf_inf.csv"
data <- read.csv(file)
data$Model <- gsub("Illumina ", "", data$Model)
data <- data[data$Platform == "ILLUMINA", ]

grouped_data <- data %>%
  group_by(Model) %>%
  summarise(Sum_Size = sum(size_MB))

grouped_data <- grouped_data %>%
  arrange(desc(Sum_Size)) %>%  
  head(5)
vector_column <- grouped_data$Model
data_another <- data[!(data$Model %in% vector_column), ]





df_combined <- rbind(grouped_data, c("Other ILLUMINA",sum(data_another$size_MB)))
df_combined$Sum_Size_TB <- round(as.numeric(df_combined$Sum_Size) / (1024*1024),0)
df_combined <- df_combined %>% arrange(Sum_Size_TB)
df_combined$Model

ordered_levels <- unique(df_combined$Model[order(df_combined$Sum_Size_TB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other ILLUMINA"])
ordered_levels <- c( ordered_levels,"Other ILLUMINA")
df_combined$Model <- factor(df_combined$Model, levels = rev(ordered_levels))


ggplot(df_combined, aes(x = as.factor(Model), y = Sum_Size_TB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  #geom_text(aes(label = Sum_Size_TB, y = Sum_Size_TB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Platform models", y = "Total Size (TB)", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),  
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    
    axis.text.x = element_text(size =12),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.y = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    breaks = c(0,100,500,1000,1750,3000,5000,7000),
    labels = c("0",'100',"500","1000","1750","3000","5000",'7000')  ) 

