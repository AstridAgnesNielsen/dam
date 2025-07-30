# Draw pie chart of married/unmarried ratio
# of men beween 20 and 30 in Århus 1801
# Astrid Agnes Nielsen, 2025

# Load in the cleaned 1801 cencus
cleaned1801 <- read.csv("census-1801-cleaned.csv")

# Create a filter to select men aged 20-30 from Århus amt
young_men_aarhus <- subset(
  cleaned1801,
  koen == "M" &
  amt == "Århus" &
  alder >= 20 & alder <= 30
)

# Categorized into labels for the pie
young_men_aarhus$marital_status <- ifelse(young_men_aarhus$civilstand == "gift", "Married", "Unmarried")

# Count and plot
counts <- table(young_men_aarhus$marital_status)
pie( counts, labels = paste(names(counts), counts, sep = ": "), main = "Married vs Unmarried men (Age 20-30, Århus, 1801)")

