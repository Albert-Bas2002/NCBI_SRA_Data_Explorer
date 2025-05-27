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
library(knitr)
library(kableExtra)
library(summarytools)


file <- "METATRANSCRIPTOMIC_data_for_some_metagenome.csv"
data <- read.csv(file)

summary(data)

data_summary <- dfSummary(data)


summary_df <- data.frame(
  Variables = names(data),
  Unique_Values = sapply(data, function(x) length(unique(x)))
)

print(summary_df)
data_summary
filtered_data <- filtered_data %>%
  mutate(Country = ifelse(grepl("BIOBOT ANALYTICS", CenterName), "United States", Country))


write.csv(filtered_data, "METATRANSCRIPTOMIC_data_for_some_metagenome.csv", row.names = FALSE)

grouped_data <- filtered_data %>%
  group_by(LibraryStrategy) %>%
  count()

grouped_data <- grouped_data %>%
  arrange(desc(n))


print(n=100,grouped_data)

kable(grouped_data, caption = "Grouped Data", format = "html") %>%
  kable_styling(full_width = FALSE)
