# Draw plot of all age groups % married men
# over all censuses

# Astrid Agnes Nielsen, 2025

# I only need some of the columns, this is mainly 
# to stop dplyr from complaining
columns_to_keep <- c("ft", "koen", "civilstand", "alder", "amt")

# Read data
data_1801 <- read.csv("census-1801-cleaned.csv", stringsAsFactors = FALSE)
data_1834 <- read.csv("census-1834-cleaned.csv", stringsAsFactors = FALSE)
data_1845 <- read.csv("census-1845-cleaned.csv", stringsAsFactors = FALSE)
data_1860 <- read.csv("census-1860-cleaned.csv", stringsAsFactors = FALSE)

# Size down to wanted columns
data_1801 <- data_1801[, columns_to_keep]
data_1834 <- data_1834[, columns_to_keep]
data_1845 <- data_1845[, columns_to_keep]
data_1860 <- data_1860[, columns_to_keep]

# Load the dplyr lib to allow joining of data
library(dplyr)

# Join data in all_data
all_data <- bind_rows(data_1801, data_1834, data_1845, data_1860)

# Make sure alder is numeric before filtering
all_data$alder <- as.numeric(all_data$alder)

# Filter and label
all_data <- all_data %>%
  mutate(age_group = case_when(
    alder >= 20 & alder < 30 ~ "20-30",
    alder >= 30 & alder < 40 ~ "30-40",
    alder >= 40              ~ "40+",
    TRUE                     ~ NA_character_
  )) %>%
  filter(!is.na(age_group), koen == "M", amt == "Århus")

# Summarize in groups
summary_data <- all_data %>%
  group_by(ft, age_group) %>%
  summarise(
    total = n(),
    married = sum(civilstand == "gift", na.rm = TRUE),
    percent_married = married / total * 100
  ) %>%
  ungroup()

# Load the plot lib
library(ggplot2)

# Create the plit
my_plot <- ggplot(summary_data, aes(x = ft, y = percent_married, color = age_group)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_x_continuous(breaks = c(1801, 1834, 1845, 1860), limits = c(1801, 1860)) +
  labs(
    title = "% of Married Men in Århus by Age Group and Year",
    x = "Census Year",
    y = "% Married",
    color = "Age Group"
  ) +
  theme_minimal()

# Save the plot
ggsave("married_men_by_agegroup.png", plot = my_plot, width = 8, height = 5, dpi = 300)
