# Draw some plots and play with data
# over different censuses

# Astrid Agnes Nielsen, 2025

# Read data
data <- read.csv("census-1801-cleaned.csv", stringsAsFactors = FALSE)

library(dplyr)

# Filter for married men aged 20+
men <- data %>%
  filter(koen == "M", alder >= 20, amt == "Århus", civilstand %in% c("gift", "ugift"))

# Make sure alder is numeric before filtering
men$alder <- as.numeric(men$alder)

men_20_29 <- men %>%
  filter(koen == "M", alder >= 20, alder < 30) %>%
  mutate(AgeSubGroup = case_when(
    alder >= 20 & alder < 25 ~ "20–24",
    alder >= 25 & alder < 30 ~ "25–29"
  ))

summary <- men_20_29 %>%
  group_by(AgeSubGroup, civilstand) %>%
  summarise(Count = n(), .groups = "drop")

summary_prop <- summary %>%
  group_by(AgeSubGroup) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()

library(ggplot2)
library(scales)

ggplot(summary_prop, aes(x = AgeSubGroup, y = Proportion, fill = civilstand)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Marital Status in Århus Men Aged 20–29 (1801)",
       x = "Age Subgroup", y = "Proportion", fill = "Marital Status") +
  theme_minimal()
