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

file_table='Sequencing_Cost_Data_Table.csv'
data_cost <- read.csv(file_table)
grouped_data <- data %>%
  group_by(ReleaseDate, ) %>%
  summarise(total_size_MB = sum(size_MB))

grouped_data <- grouped_data[grouped_data$ReleaseDate != 0, ]

grouped_data$total_size_TB <- grouped_data$total_size_MB / (1024*1024) 
data_cost$Date <- as.Date(data_cost$Date, format = "%m/%d/%Y")
grouped_data$ReleaseDate <- as.Date(paste0(grouped_data$ReleaseDate, "-01-01"))

all_years <- seq(from = as.Date("2001-01-01"), to = as.Date("2022-01-01"), by = "years")
odd_indices <- seq(2, length(all_years), by = 2) 
all_years <- all_years[odd_indices] 
ggplot(data_cost) +
  geom_line(aes(x = Date, y = Cost.per.Mb), color = "#8da0cb",size=1) +
  geom_line(data = grouped_data, aes(x = ReleaseDate, y = total_size_TB), color = "#bc80bd",size=1) +
  
  labs(x = "", y = "", title = "") +
  theme_classic() +  
  scale_x_date(breaks = all_years, date_labels = "%Y",expand = expansion(add = c(0,0))) +  
  
  scale_y_continuous(
    name = "\nCost per Mb (log scale)",
    trans = "log10", 
    expand = expansion(add = c(0,0)), 
    
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000,10000),
    labels = c('', '0.010$', '0.100$', '1$', '10$', '100$', '1000$', '10000$'),
    sec.axis = sec_axis(~.,name='\nTotal Size TB (log scale)',breaks = c( 0.1, 1, 10, 100,1000, 3700),
                        labels = c('0.100', '1', '10','100','1000', '3700'))
    
  ) +
  

  
  expand_limits(y = c(0.001, 10000)) +
  
  theme(axis.title.y.right = element_text(size=16,color = "#bc80bd"),
        axis.title.y.left = element_text(size=16,color = "#8da0cb"),
        axis.text.y = element_text(size = 12),
        axis.text.x= element_text(size = 10),
        
        
        panel.grid.major = element_line(color = "gray", linetype = "dashed"))
  
