# =============================================================================
# 30_position_player_analysis.R
# Position Player Analysis for Rule 5 Draft
# Separates pitchers from hitters, then classifies by position
# =============================================================================

library(dplyr)
library(tidyr)

cat("Separating pitchers and hitters from mastersheet...\n")

# Load full mastersheet
mastersheet <- read.csv("data/Fangraphs_Mastersheet - Hitters.csv", stringsAsFactors = FALSE)

# Separate pitchers (SP, RP, SP/RP, RP/SP, SP/DH - two-way players)
pitchers <- mastersheet %>%
  filter(POS %in% c("RP", "SP", "SP/RP", "RP/SP", "SP/DH", "RP/DH"))

# Everyone else is a hitter
hitters <- mastersheet %>%
  filter(!POS %in% c("RP", "SP", "SP/RP", "RP/SP", "SP/DH", "RP/DH"))

cat(sprintf("✅ Separated: %d pitchers, %d hitters\n", nrow(pitchers), nrow(hitters)))

# Save separated files
write.csv(pitchers, "data/Pitching_Prospects_Mastersheet.csv", row.names = FALSE)
write.csv(hitters, "data/Hitters_Mastersheet_Clean.csv", row.names = FALSE)

cat("\n📁 Files created:\n")
cat("   - data/Pitching_Prospects_Mastersheet.csv (pitchers backup)\n")
cat("   - data/Hitters_Mastersheet_Clean.csv (hitters only)\n")

# Filter for Rule 5 eligible hitters only
hitters_r5 <- hitters %>%
  filter(Options.or.R5.Status == "R5" | grepl("R5", Options.or.R5.Status)) %>%
  filter(!is.na(POS) & POS != "")

cat(sprintf("\n🎯 Found %d Rule 5 eligible position players\n", nrow(hitters_r5)))

# Show position distribution
cat("\nPosition combinations in R5 eligible hitters:\n")
pos_counts <- hitters_r5 %>%
  group_by(POS) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(head(pos_counts, 30))

# Save R5 eligible hitters
write.csv(hitters_r5, "output/position_players_r5_eligible.csv", row.names = FALSE)

cat("\n✅ R5 eligible hitters saved to: output/position_players_r5_eligible.csv\n")
