# 39_rank_position_players.R
# Apply R5 benchmarks to rank position players
# Filter to AA/AAA 2023-2025, find Top 3 per position

library(dplyr)
library(tidyr)

cat("\n🎯 RANKING POSITION PLAYERS USING R5 BENCHMARKS\n\n")

# Load scraped data
all_stats <- read.csv("data/all_hitters_milb_2023_2025.csv", stringsAsFactors = FALSE)

# Load mastersheet for defensive positions
mastersheet <- read.csv("data/Hitters_Mastersheet_Clean.csv", stringsAsFactors = FALSE)

cat("✅ Loaded", nrow(all_stats), "season records for", 
    length(unique(all_stats$player_name)), "players\n\n")

# Use SUGGESTED thresholds (more aggressive - players good enough to KEEP, not just draft)
threshold_wRC_plus <- 85
threshold_wOBA <- 0.320
threshold_ISO <- 0.130
threshold_OBP <- 0.320
threshold_min_PA <- 250

cat("📊 Suggested Thresholds (Players to KEEP on 40-man):\n")
cat("   wRC+:  ", threshold_wRC_plus, "+\n")
cat("   wOBA:  ", threshold_wOBA, "+\n")
cat("   ISO:   ", threshold_ISO, "+\n")
cat("   OBP:   ", threshold_OBP, "+\n")
cat("   Min PA:", threshold_min_PA, "\n\n")

# Clean column names by position
names(all_stats)[1] <- "Season"
names(all_stats)[2] <- "Team" 
names(all_stats)[3] <- "Level"
names(all_stats)[4] <- "Age"
names(all_stats)[5] <- "G"
names(all_stats)[6] <- "PA"
names(all_stats)[7] <- "HR"
names(all_stats)[8] <- "R"
names(all_stats)[9] <- "RBI"
names(all_stats)[10] <- "SB"
names(all_stats)[12] <- "BB_pct"
names(all_stats)[13] <- "K_pct"
names(all_stats)[14] <- "ISO"
names(all_stats)[15] <- "BABIP"
names(all_stats)[17] <- "AVG"
names(all_stats)[18] <- "OBP"
names(all_stats)[19] <- "SLG"
names(all_stats)[20] <- "wOBA"
names(all_stats)[21] <- "xwOBA"
names(all_stats)[22] <- "wRC_plus"
names(all_stats)[28] <- "WAR"
names(all_stats)[29] <- "playerId"
names(all_stats)[30] <- "player_name"
names(all_stats)[31] <- "AB"
names(all_stats)[32] <- "H"
names(all_stats)[33] <- "X1B"
names(all_stats)[34] <- "X2B"
names(all_stats)[35] <- "X3B"
names(all_stats)[43] <- "CS"
names(all_stats)[46] <- "OPS"
names(all_stats)[48] <- "UBR"

# Filter to AA/AAA 2023-2025 and clean
player_stats <- all_stats %>%
  filter(Season %in% c(2023, 2024, 2025)) %>%
  filter(Level %in% c("AA", "AAA")) %>%
  mutate(
    BB_pct = as.numeric(gsub("%", "", BB_pct)),
    K_pct = as.numeric(gsub("%", "", K_pct)),
    PA = as.numeric(PA),
    wOBA = as.numeric(wOBA),
    wRC_plus = as.numeric(wRC_plus),
    ISO = as.numeric(ISO),
    AVG = as.numeric(AVG),
    OBP = as.numeric(OBP),
    SLG = as.numeric(SLG),
    SB = as.numeric(SB),
    HR = as.numeric(HR),
    Age = as.numeric(Age),
    X3B = as.numeric(X3B),
    CS = as.numeric(CS),
    OPS = as.numeric(OPS),
    UBR = as.numeric(UBR)
  ) %>%
  filter(!is.na(wOBA), !is.na(PA), PA > 0) %>%
  distinct(Season, Team, Level, playerId, position_bucket, G, PA, .keep_all = TRUE)

cat("🎯 Filtered to AA/AAA 2023-2025 with valid stats:\n")
cat("   Records:", nrow(player_stats), "\n")
cat("   Players:", length(unique(player_stats$player_name)), "\n\n")

# Calculate league average OPS for OPS+ calculation
league_avg_OPS <- player_stats %>%
  summarize(avg_OPS = weighted.mean(OPS, PA, na.rm = TRUE)) %>%
  pull(avg_OPS)

cat("📊 League Average OPS (AA/AAA 2023-2025):", round(league_avg_OPS, 3), "\n\n")

# Aggregate by player
player_rankings <- player_stats %>%
  group_by(player_name, playerId, position_bucket, org) %>%
  summarize(
    seasons = n(),
    total_PA = sum(PA, na.rm = TRUE),
    avg_age = mean(Age, na.rm = TRUE),
    
    # Offensive metrics (weighted by PA)
    wOBA = weighted.mean(wOBA, PA, na.rm = TRUE),
    wRC_plus = weighted.mean(wRC_plus, PA, na.rm = TRUE),
    ISO = weighted.mean(ISO, PA, na.rm = TRUE),
    AVG = weighted.mean(AVG, PA, na.rm = TRUE),
    OBP = weighted.mean(OBP, PA, na.rm = TRUE),
    SLG = weighted.mean(SLG, PA, na.rm = TRUE),
    OPS = weighted.mean(OPS, PA, na.rm = TRUE),
    
    # Plate discipline
    BB_pct = weighted.mean(BB_pct, PA, na.rm = TRUE),
    K_pct = weighted.mean(K_pct, PA, na.rm = TRUE),
    
    # Power/Speed
    HR = sum(HR, na.rm = TRUE),
    SB = sum(SB, na.rm = TRUE),
    CS = sum(CS, na.rm = TRUE),
    X3B = sum(X3B, na.rm = TRUE),
    UBR = sum(UBR, na.rm = TRUE),  # Ultimate Base Running (usually NA in MiLB)
    
    .groups = "drop"
  ) %>%
  # Join defensive positions from mastersheet
  left_join(
    mastersheet %>% select(playerId, POS),
    by = "playerId"
  ) %>%
  # Calculate tool rates
  mutate(
    SB_rate = SB / total_PA,
    HR_rate = HR / total_PA,
    X3B_rate = X3B / total_PA,
    OPS_plus = (OPS / league_avg_OPS) * 100,  # OPS+ normalized to 100
    SB_success = ifelse((SB + CS) > 0, SB / (SB + CS), NA)  # Stolen base success rate
  ) %>%
  # Apply suggested thresholds
  filter(
    total_PA >= threshold_min_PA,
    wRC_plus >= threshold_wRC_plus,
    wOBA >= threshold_wOBA,
    ISO >= threshold_ISO,
    OBP >= threshold_OBP
  ) %>%
  # Create composite score with tool bonuses
  mutate(
    # Base composite score
    composite_score = (wRC_plus * 0.40) +      # 40% offensive value
                      (wOBA * 200 * 0.25) +     # 25% offensive skill (scale wOBA)
                      (ISO * 200 * 0.15) +      # 15% power
                      (OBP * 100 * 0.10) +      # 10% on-base ability  
                      (BB_pct * 2 * 0.05) +     # 5% plate discipline
                      ((100 - K_pct) * 0.05),   # 5% contact ability
    
    # SPEED TOOL BONUSES (max 10 points)
    speed_bonus = case_when(
      SB_rate >= 0.08 ~ 10,                    # Elite stealer (80-grade speed)
      SB_rate >= 0.06 ~ 7,                     # Plus-plus speed
      SB_rate >= 0.04 ~ 4,                     # Above-average speed
      TRUE ~ 0
    ) + ifelse(X3B_rate >= 0.015, 3, 0),       # Triples = speed to stretch doubles
    
    # POWER TOOL BONUSES (max 10 points)
    power_bonus = case_when(
      HR_rate >= 0.04 ~ 10,                    # Elite power (30+ HR pace over 650 PA)
      HR_rate >= 0.03 ~ 7,                     # Plus-plus power (20+ HR)
      HR_rate >= 0.02 ~ 4,                     # Above-average power
      TRUE ~ 0
    ) + ifelse(ISO >= 0.200, 3, 0),            # Premium raw power
    
    # TOTAL SCORE = Base + Speed + Power (no defense bonus)
    total_score = composite_score + speed_bonus + power_bonus
  ) %>%
  arrange(desc(total_score))

cat("🔥 Players meeting suggested thresholds:", nrow(player_rankings), "\n\n")

# Rank within each position
position_rankings <- player_rankings %>%
  group_by(position_bucket) %>%
  mutate(position_rank = row_number()) %>%
  ungroup() %>%
  arrange(position_bucket, position_rank)

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  📊 TOP 3 PER POSITION (R5 Draft Targets)\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")



# Output overall Top 25 players regardless of position
cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  🏆 OVERALL TOP 25 POSITION PLAYERS\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")
top25 <- player_rankings %>% arrange(desc(total_score)) %>% head(25)
for (i in seq_len(nrow(top25))) {
  p <- top25[i, ]
  cat(" ", i, ".", p$player_name, "-", p$org, "(", p$position_bucket, ")\n")
  cat("     wRC+:", round(p$wRC_plus, 0), "| wOBA:", round(p$wOBA, 3),
      "| ISO:", round(p$ISO, 3), "| OBP:", round(p$OBP, 3), "\n")
  cat("     PA:", p$total_PA, "| Age:", round(p$avg_age, 1),
      "| HR:", p$HR, "| SB:", p$SB, "\n")
  if (p$speed_bonus > 0 || p$power_bonus > 0) {
    cat("     🔥 TOOLS: Speed +", round(p$speed_bonus, 1), "| Power +", round(p$power_bonus, 1), "\n")
  }
  if (!is.na(p$OPS_plus)) {
    cat("     📊 OPS+:", round(p$OPS_plus, 0), "| OPS:", round(p$OPS, 3), "\n")
  }
}
cat("\n")

# Save rankings

write.csv(player_rankings, "output/position_player_rankings_all.csv", row.names = FALSE)
write.csv(top25, "output/position_player_top25_overall.csv", row.names = FALSE)
write.csv(position_rankings %>% filter(position_rank <= 5), 
          "output/position_player_top5_per_position.csv", row.names = FALSE)
write.csv(position_rankings %>% filter(position_rank > 5 & position_rank <= 7), 
          "output/position_player_honorable_mentions.csv", row.names = FALSE)

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  ✅ RANKING COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  Total qualified players:", nrow(player_rankings), "\n")
cat("  Players in Top 3 per position:", nrow(position_rankings %>% filter(position_rank <= 3)), "\n")
cat("  📁 All rankings: output/position_player_rankings_all.csv\n")
cat("  📁 Top 3 per position: output/position_player_top3_per_position.csv\n\n")

cat("🏁 WE FUCKING DID IT! Position player targets identified!\n")
