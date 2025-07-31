# Draw some plots and play with data
# over different censuses

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
library(tidyr)
library(scales)

# Join data in all_data
all_data <- bind_rows(data_1801, data_1834, data_1845, data_1860)

# Filter for married men aged 20+, only 1801 and 1860
df_men <- all_data %>%
  filter(koen == "M", civilstand == "gift", alder >= 20, amt == "Århus", ft %in% c(1801, 1860))

# Make sure alder is numeric before filtering
all_data$alder <- as.numeric(all_data$alder)

# Create age groups
df_men <- df_men %>%
  mutate(AgeGroup = case_when(
    alder >= 20 & alder < 30 ~ "20–29",
    alder >= 30 & alder < 40 ~ "30–39",
    alder >= 40            ~ "40+"
  ))

# Count by year and age group
age_group_counts <- df_men %>%
  group_by(ft, AgeGroup) %>%
  summarise(Count = n(), .groups = "drop")

# Total married men per year for proportion
total_counts <- df_men %>%
  group_by(ft) %>%
  summarise(Total = n())

# Merge for proportions
age_group_props <- age_group_counts %>%
  left_join(total_counts, by = "ft") %>%
  mutate(Proportion = Count / Total)

library(ggplot2)

# Plot bar charts in absolute numbers
ggplot(age_group_counts, aes(x = AgeGroup, y = Count, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ ft) +
  labs(title = "Married Men in Århus by Age Group", x = "Age Group", y = "Number of Married Men") +
  theme_minimal()

# Plot proportional bar charts
ggplot(age_group_props, aes(x = factor(ft), y = Proportion, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Age Group Proportions of Married Men: 1801 vs 1860",
       x = "ft", y = "Proportion", fill = "Age Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Filter for married males under 20
filtered_young_1860 <- all_data %>%
  filter(koen == "M", 
         civilstand == "gift", 
	 amt == "Århus",
         ft == 1860,
         alder < 20, alder > 9)

ggplot(filtered_young_1860, aes(x = alder)) +
  geom_histogram(binwidth = 1, fill = "#4B9CD3", color = "black") +
  scale_x_continuous(breaks = 10:20) +
  labs(title = "Married Males by Age (Under 20)",
       x = "Age",
       y = "Number of Married Males") +
  theme_minimal()

# Create contingency tablem this can show if there are significant statistical
# changes in the agegroups over ft
# Pearson's Chi-squared test
ctable <- table(df_men$ft, df_men$AgeGroup)
chisq.test(ctable)

# Count total married men per year and per age group
proportions_table <- df_men %>%
  group_by(ft, AgeGroup) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(ft) %>%
  mutate(Total = sum(Count),
         Proportion = Count / Total) %>%
  ungroup()

# Pivot to show AgeGroups as rows and Years as columns
proportions_wide <- proportions_table %>%
  select(ft, AgeGroup, Proportion) %>%
  pivot_wider(names_from = ft, values_from = Proportion)

# Print with percentage formatting, this way i get the shift
# in distribution in real numbers
proportions_wide %>%
  mutate(across(where(is.numeric), ~ percent(.x, accuracy = 0.1)))

# Youngest married men pr year
all_data %>%
  filter(koen == "M", civilstand == "gift", amt == "Århus", alder > 9, ft > 0) %>%
  group_by(ft) %>%
  summarise(EarliestAge = min(alder, na.rm = TRUE))

## Data test for 1860 to see youngest gift, alder
all_data %>%
  filter(ft == 1860, koen == "M", civilstand == "gift", alder < 20, amt == "Århus") %>%
  arrange(alder)


# Most men are married? - proportions...
# Filter only men aged 20+
df_men <- all_data %>%
  filter(koen == "M", alder >= 20, amt == "Århus", ft > 0)

# Count married and total men by year
marriage_summary <- df_men %>%
  group_by(ft) %>%
  summarise(
    TotalMen = n(),
    MarriedMen = sum(civilstand == "gift", na.rm = TRUE),
    MarriageRate = MarriedMen / TotalMen
  )

# See which year had the highest proportion married
marriage_summary %>%
  arrange(desc(MarriageRate))
