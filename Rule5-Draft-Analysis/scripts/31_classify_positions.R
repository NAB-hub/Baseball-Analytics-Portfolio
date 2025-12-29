# =============================================================================
# 31_classify_positions.R
# Classify Rule 5 eligible hitters into position groups
# C, 1B, 2B, 3B, SS, OF, UTL
# =============================================================================

library(dplyr)

cat("Classifying position players into buckets...\n")

# Load R5 eligible hitters
hitters <- read.csv("output/position_players_r5_eligible.csv", stringsAsFactors = FALSE)

# Parse position information
hitters <- hitters %>%
  mutate(
    primary_position = sapply(strsplit(POS, "/"), function(x) x[1]),
    num_positions = sapply(strsplit(POS, "/"), length),
    has_C = grepl("\\bC\\b", POS),
    has_1B = grepl("1B", POS),
    has_2B = grepl("2B", POS),
    has_3B = grepl("3B", POS),
    has_SS = grepl("SS", POS),
    has_LF = grepl("LF", POS),
    has_CF = grepl("CF", POS),
    has_RF = grepl("RF", POS),
    has_any_OF = has_LF | has_CF | has_RF,
    has_any_IF = has_1B | has_2B | has_3B | has_SS
  )

# Classification logic:
# UTL = C + anything else OR IF + OF combination
# Otherwise = primary position
hitters <- hitters %>%
  mutate(
    position_bucket = case_when(
      # Utility: Catcher who plays other positions OR infielder who plays OF
      (has_C & num_positions > 1) | (has_any_IF & has_any_OF) ~ "UTL",
      # Pure positions (primary position determines bucket)
      primary_position == "C" ~ "C",
      primary_position == "1B" ~ "1B",
      primary_position == "2B" ~ "2B",
      primary_position == "3B" ~ "3B",
      primary_position == "SS" ~ "SS",
      primary_position %in% c("LF", "CF", "RF") ~ "OF",
      # Multi-position within same zone
      has_any_OF & !has_C & !has_any_IF ~ "OF",
      has_1B & !has_C & !has_any_OF ~ "1B",
      has_2B & !has_C & !has_any_OF ~ "2B",
      has_3B & !has_C & !has_any_OF ~ "3B",
      has_SS & !has_C & !has_any_OF ~ "SS",
      TRUE ~ "OTHER"
    )
  )

# Count by bucket
bucket_counts <- hitters %>%
  group_by(position_bucket) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\n📊 Position Bucket Distribution:\n")
print(bucket_counts)

# Save classified data
write.csv(hitters, "output/position_players_classified.csv", row.names = FALSE)

cat("\n✅ Classification complete!\n")
cat("   📁 Output: output/position_players_classified.csv\n")
cat(sprintf("   🎯 Total: %d players across %d position buckets\n", nrow(hitters), nrow(bucket_counts)))
