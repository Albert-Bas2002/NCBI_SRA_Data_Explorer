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
grouped_data <- data %>%
  group_by(Model,Platform) %>%
  summarise(Run = n())
df <- grouped_data %>%
  arrange(desc(Run)) %>%  
  head(10)
vector_column <- df$Model
data <- data[data$Model %in% vector_column, ]

data$size_range <- cut(data$size_MB,
                       breaks = c(0, 100, 1000, 10000,40000, Inf),
                       labels = c("0-100", "100-1000", "1000-10000","10000-40000", "40000+"),
                       include.lowest = TRUE)

result_table <- data %>%
  group_by(Model,Platform, size_range) %>%
  summarise(count = n())
result_table_percent <- result_table %>%
  group_by(Model) %>%
  mutate(count = round(count / sum(count) * 100, 1))

df_spread <- spread(result_table_percent, key = size_range, value = count)
kable(df_spread, format = "html", caption = "")


result_table_percent$size_range <- factor(result_table_percent$size_range, levels = c( "40000+","10000-40000",'1000-10000',"100-1000", "0-100"))
result_table_percent$Model <- factor(result_table_percent$Model, levels = c("Sequel II", "GridION", "Illumina MiSeq", "NextSeq 550", "Illumina NovaSeq 6000", "NextSeq 500", "Illumina HiSeq 2500", "Illumina HiSeq 2000", "Illumina HiSeq 4000", "HiSeq X Ten")
                                     
                                     
                                     
                                     
                                     
)
ggplot(result_table_percent, aes(x = Model, y = count, fill = size_range)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +  
  labs(title = "",y='Percentage (%)',x='Platform models',fill = 'Size Range (MB)') +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1), 
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 12),   
    legend.title = element_text(size = 12), 
    
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    panel.grid.major.y = element_blank(),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.position = c(-0.275, 0.05),
    panel.grid.major.x = element_line(size = 0.1,color = 'black'),
    
    
    legend.background = element_rect(color = "black", size = 0.2)
    
    
  )+scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f"
                                 
                                 
                                 
                                 
                                 
                              
  ))
