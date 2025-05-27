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
library(scales)


sqrt3_trans <- function() {
  trans_new(name = 'sqrt3',
            transform = function(x) x^(1/3),
            inverse = function(x) x^3)
}

file <- "SRA_graf_inf.csv"
data <- read.csv(file)
data <- data[data$ReleaseDate != 0, ]
data <- data[data$ReleaseDate > 2015, ]
data$Model <- gsub("Illumina ", "", data$Model)

grouped_data <- data %>%
  filter(Platform=='ILLUMINA')%>%
  group_by(Model) %>%
  summarise(Run = n())

df <- grouped_data %>%
  arrange(desc(Run)) %>%  
  head(9)
vector_column <- df$Model
data_another <- data[!(data$Model %in% vector_column), ]

grouped_data_another <- data_another %>%
  group_by(ReleaseDate) %>%
  summarise(Run = n())
grouped_data_another$Model <- "All Other Models"



data <- data[data$Model %in% vector_column, ]

grouped_data <- data %>%
  group_by(ReleaseDate, Model) %>%
  summarise(Run = n())

df_combined <- rbind( grouped_data_another,grouped_data)
df_combined <- df_combined %>%
  group_by(ReleaseDate) %>%
  mutate(TotalRun = sum(Run), 
         Percentage = (Run / TotalRun) * 100) %>%
  select(-TotalRun)


df_combined$Percentage <- paste0('— (~',round(df_combined$Percentage, 2), "%)")
unique(df_combined$Model)

df_combined <- df_combined %>%
  mutate(Percentage = if_else(Model != "All Other Models", '', Percentage))
df_combined$Model <- factor(df_combined$Model, levels = c("All Other Models","HiSeq 3000" , "HiSeq X Ten","HiSeq 2000" , "NextSeq 550","HiSeq 4000" , "NextSeq 500","HiSeq 2500" , "MiSeq", "NovaSeq 6000"))

ggplot(df_combined, aes(x = as.factor(ReleaseDate), y = Run, fill = Model)) +
  geom_hline(yintercept = c(0,10000, 100000, 500000, 1000000,1500000,2000000, 2500000), linetype = "dashed", color = "gray") +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.8), alpha = 0.95, color = "black") +
  
  coord_flip() +
  
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
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.80, 0.30),
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_fill_manual("ILLUMINA platform models",breaks=c("NovaSeq 6000", "MiSeq","HiSeq 4000",   "NextSeq 500","HiSeq 2500", "NextSeq 550","HiSeq X Ten",  "HiSeq 2000", "HiSeq 3000", "All Other Models"),
                    values = c(
                      "#ffff66", "#5fbe5d", "#4b8fd1", "#a770b5", "#ff9333", "#f43d3f", "#b96d3b", "#ff93d1","#1b9e77", "#aaaaaa"
                    )) +
  
  geom_text(aes(label = Percentage,y=Run),angle=0, position = position_dodge(width = 0.7), hjust = -0.1, vjust = 0.60, size = 3) +
  scale_y_continuous(
    trans='sqrt',
    breaks = c(10000,100000,500000,1000000,1500000,2000000,2500000),
    labels = c( "10k", "100k", "500k",'1000k', "1500k",'2000k', "2500k")  ) +
  labs(x = "", y = "Runs", title = "") 

