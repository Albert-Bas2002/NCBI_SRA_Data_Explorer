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
  summarise(Run = n())

grouped_data <- grouped_data %>%
  arrange(desc(Run)) %>% 
  head(15)
vector_column <- grouped_data$Model
vector_column
data_another <- data[!(data$Model %in% vector_column), ]





df_combined <- rbind(grouped_data, c("Other ILLUMINA",nrow(data_another)))
df_combined <- df_combined %>% arrange(as.integer(Run))
df_combined$Run<-as.integer(df_combined$Run)
df_combined$Model
unique(df_combined$Model[order(df_combined$Run)])

ordered_levels <- unique(df_combined$Model[order(df_combined$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other ILLUMINA"])
ordered_levels <- c( ordered_levels,"Other ILLUMINA")
df_combined$Model <- factor(df_combined$Model, levels = rev(ordered_levels))

ggplot(df_combined, aes(x = as.factor(Model), y = as.integer(Run))) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(as.integer(Run / 1000), nsmall = 0), "k"), y = as.integer(Run)), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "", y = "", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1), 
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),   
    legend.title = element_text(size = 12),  
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2))+
  scale_y_continuous(
    trans='sqrt',
    expand = c(0.01,0.5),
    breaks = c(0,100000,500000,1000000,1750000,3000000,5000000,7000000),
    labels = c("0",'100k',"500k","1000k","1750k","3000k","5000k",'7000k')  ) 

