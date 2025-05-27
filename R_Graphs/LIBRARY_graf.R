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


grouped_data_source <- data %>%
  group_by(Country) %>%
  summarise(Run = n())

grouped_data_source <- data %>%
  group_by(LibrarySource) %>%
  summarise(Run = n(), .groups = 'drop') %>%
  mutate(TotalRuns = sum(Run, na.rm = TRUE))
grouped_data_source$PercentageForSource <- (grouped_data_source$Run / grouped_data_source$TotalRuns) * 100


grouped_data <- data %>%
  group_by(LibrarySource, LibraryStrategy) %>%
  summarise(Run = n(), .groups = 'drop')

grouped_data <- grouped_data %>%
  group_by(LibrarySource) %>%
  mutate(Rank = dense_rank(desc(Run))) %>%
  mutate(LibraryStrategy = ifelse(Rank <= 10, LibraryStrategy, "OTHER")) %>%
  select(-Rank)


grouped_data_last <- grouped_data %>%
  group_by(LibrarySource, LibraryStrategy) %>%
  summarise(SumRun = sum(Run))

grouped_data_last  <- grouped_data_last  %>%
  group_by(LibrarySource) %>%
  mutate(Run_Percentage = (SumRun / sum(SumRun)) * 100)%>%
  arrange(desc(Run_Percentage))  

merged_data <- merge(grouped_data_source, grouped_data_last, by = "LibrarySource", all.x = TRUE)
merged_data <- merged_data %>%
  mutate(PercentageForSource = ifelse(LibraryStrategy == "OTHER", PercentageForSource, NaN))

ordered_levels <- unique(merged_data$LibraryStrategy[order(merged_data$PercentageForSource)])
ordered_levels <- rev(ordered_levels[ordered_levels != "OTHER"])
ordered_levels <- c( ordered_levels,"OTHER")
ordered_levels
merged_data$LibraryStrategy <- factor(merged_data$LibraryStrategy, levels = rev(ordered_levels))


merged_data <- merged_data %>%
  mutate(LibrarySource = case_when(
    LibrarySource == "GENOMIC"                    ~ "GENOMIC (37.6%)",
    LibrarySource == "VIRAL RNA"                  ~ "VIRAL RNA (23.4%)",
    LibrarySource == "TRANSCRIPTOMIC"             ~ "TRANSCRIPTOMIC (18.2%)",
    LibrarySource == "METAGENOMIC"                ~ "METAGENOMIC (17.5%)",
    LibrarySource == "TRANSCRIPTOMIC SINGLE CELL" ~ "TRANSCRIPTOMIC SINGLE CELL (1.15%)",
    LibrarySource == "METATRANSCRIPTOMIC"         ~ "METATRANSCRIPTOMIC (0.47%)",
    LibrarySource == "SYNTHETIC"                  ~ "SYNTHETIC (0.37%)",
    LibrarySource == "GENOMIC SINGLE CELL"        ~ "GENOMIC SINGLE CELL (0.27%)",
    TRUE                                          ~ "OTHER Source (0.85%)"
  ))


ordered_levels <- unique(merged_data$LibrarySource[order(merged_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "OTHER Source. Percent of total Runs (0.85%)"])
ordered_levels <- c( ordered_levels,"OTHER Source. Percent of total Runs (0.85%)")
ordered_levels
merged_data$LibrarySource <- factor(merged_data$LibrarySource, levels = ordered_levels)




gg <- ggplot(merged_data, aes(x = LibraryStrategy, y = Run_Percentage)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  #geom_text(aes(label = paste0("-",round(Run_Percentage,1), "%"), y = as.integer(Run_Percentage)), position = position_dodge(width = 0.6), hjust = 0, vjust = 0, size = 2.5) +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  facet_wrap(~ LibrarySource, ncol = 3, scales = "free", drop = FALSE) +
  coord_flip() +
  labs(x = "Library Strategy", y = "Percentage (scale sqrt)", title = "Run Percentage by LibraryStrategy") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =7),
    axis.text.y = element_text(size = 7),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size =12),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(trans='sqrt',limits=c(0,100),breaks = c(1,5,15,30,50,75,100))
  


# Добавляем надписи по колонке PercentageForSource

print(gg)
 