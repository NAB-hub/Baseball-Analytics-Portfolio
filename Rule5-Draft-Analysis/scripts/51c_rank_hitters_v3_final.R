# =============================================================================
# 51c_rank_hitters_v3_final.R
# FINAL Hitter Ranking - Incorporating Defensive Value + Platoon Analysis
# 
# V3 Updates:
# 1. Defensive positional multiplier (Catchers boosted significantly)
# 2. Platoon split penalty (RHH with pronounced splits downgraded)
# 3. One-position corner players penalized
# 4. Injury flag for Carson Taylor
# =============================================================================

library(dplyr)

cat("🎯 FINAL Hitter Ranking (V3: Defense + Platoon Analysis)...\n\n")

# Load V2 rankings
rankings_v2 <- read.csv("output/hitter_rankings_v2.csv", stringsAsFactors = FALSE)

# Load defensive analysis
defensive <- read.csv("output/defensive_value_analysis.csv", stringsAsFactors = FALSE)

# Manual platoon data from research
platoon_adjustments <- data.frame(
  player_name = c("Carlos Mendoza", "Felix Reyes", "Connor Charping", 
                  "Victor Labrada", "Yohendrick Pinango"),
  bats = c("L", "R", "R", "L", "L"),
  platoon_severity = c("MODERATE", "SEVERE", "MODERATE", "MODERATE", "MINIMAL"),
  platoon_notes = c(
    "LHH - better vs RHP, sheltered vs LHP",
    "RHH - pronounced splits, destroys LHP, struggles vs RHP",
    "RHH - usual RHH vulnerability vs RHP",
    "LHH - does damage vs RHP, not unusable vs LHH",
    "LHH - similar OPS vs both sides, NO severe split"
  ),
  stringsAsFactors = FALSE
)

# Injury data
injury_flags <- data.frame(
  player_name = c("Carson Taylor"),
  injury_status = c("CRITICAL"),
  injury_notes = c("Right-hand labrum surgery April 2025, out entire season, age 27 in 2026"),
  stringsAsFactors = FALSE
)

cat("⚙️  Applying V3 Adjustments...\n\n")

# Merge all data
rankings_v3 <- rankings_v2 %>%
  left_join(defensive %>% select(player_name, defensive_tier, versatility_score, r5_defensive_value),
            by = "player_name") %>%
  left_join(platoon_adjustments, by = "player_name") %>%
  left_join(injury_flags, by = "player_name") %>%
  mutate(
    # DEFENSIVE VALUE MULTIPLIER (new for V3)
    defensive_multiplier = case_when(
      latest_position == "C" ~ 1.30,              # Catchers = ELITE value
      latest_position == "UTL" ~ 1.20,            # True utility = HIGH value
      latest_position %in% c("SS", "CF") ~ 1.15,  # Premium up-middle
      latest_position == "2B" ~ 1.10,             # Above average
      latest_position == "3B" ~ 1.05,             # Average versatile
      latest_position == "OF" ~ 1.00,             # Corner OF baseline
      latest_position == "1B" & recent_wrc >= 130 ~ 1.00,  # Elite-hitting 1B
      latest_position == "1B" & recent_wrc < 130 ~ 0.80,   # PENALTY for weak 1B
      TRUE ~ 0.95
    ),
    
    # PLATOON PENALTY (new for V3)
    platoon_multiplier = case_when(
      platoon_severity == "MINIMAL" ~ 1.10,    # BONUS for no splits!
      platoon_severity == "MODERATE" ~ 1.00,   # Normal, expected
      platoon_severity == "SEVERE" & latest_position %in% c("1B", "OF", "DH") ~ 0.95,  # Can platoon
      platoon_severity == "SEVERE" & !latest_position %in% c("1B", "OF", "DH") ~ 0.85, # Can't hide
      is.na(platoon_severity) ~ 1.00,  # Unknown, assume normal
      TRUE ~ 1.00
    ),
    
    # INJURY PENALTY (new for V3)
    injury_multiplier = case_when(
      injury_status == "CRITICAL" ~ 0.50,  # Major surgery, out entire year
      injury_status == "HIGH" ~ 0.85,      # Significant injury history
      injury_status == "MODERATE" ~ 0.95,  # Minor concerns
      TRUE ~ 1.00  # Healthy
    ),
    
    # RECALCULATE COMPOSITE with all multipliers
    composite_score_v3 = base_score * position_value * utility_bonus * 
                         defensive_multiplier * platoon_multiplier * injury_multiplier,
    
    # Add context flags
    v3_boost = case_when(
      !is.na(defensive_multiplier) & defensive_multiplier >= 1.20 ~ "🔼 Defense Boost",
      !is.na(platoon_multiplier) & platoon_multiplier >= 1.10 ~ "🔼 No Platoon Split",
      TRUE ~ ""
    ),
    
    v3_penalty = case_when(
      !is.na(injury_multiplier) & injury_multiplier < 1.00 ~ "🔻 Injury Risk",
      !is.na(platoon_multiplier) & platoon_multiplier < 1.00 ~ "🔻 Platoon Concern",
      !is.na(defensive_multiplier) & defensive_multiplier < 1.00 ~ "🔻 Weak Defense",
      TRUE ~ ""
    ),
    
    v3_flags = paste(v3_boost, v3_penalty) %>% trimws()
  ) %>%
  arrange(desc(composite_score_v3)) %>%
  mutate(rank_v3 = row_number(),
         rank_change = rank - rank_v3)

# Save full V3 rankings
write.csv(rankings_v3, "output/hitter_rankings_v3_final.csv", row.names = FALSE)

cat("✅ V3 Rankings Complete! Saved to output/hitter_rankings_v3_final.csv\n\n")

# Show top 15 with changes
cat("🏆 TOP 15 FINAL RANKINGS (V3):\n\n")

top15_v3 <- rankings_v3 %>%
  select(rank_v3, player_name, org, latest_position, latest_age,
         recent_wrc, composite_score_v3, rank_change,
         defensive_tier, platoon_severity, injury_status,
         mlb_ready, v3_flags) %>%
  head(15)

print(top15_v3, row.names = FALSE)

# Show biggest movers
cat("\n\n📈 BIGGEST RISERS (V2 → V3):\n\n")

risers <- rankings_v3 %>%
  filter(rank_change >= 3) %>%
  select(rank_v3, player_name, latest_position, rank_change, 
         defensive_tier, platoon_severity, v3_flags) %>%
  arrange(desc(rank_change))

if (nrow(risers) > 0) {
  print(risers, row.names = FALSE)
} else {
  cat("   No players rose 3+ spots\n")
}

cat("\n\n📉 BIGGEST FALLERS (V2 → V3):\n\n")

fallers <- rankings_v3 %>%
  filter(rank_change <= -3) %>%
  select(rank_v3, player_name, latest_position, rank_change,
         defensive_tier, platoon_severity, injury_status, v3_flags) %>%
  arrange(rank_change)

if (nrow(fallers) > 0) {
  print(fallers, row.names = FALSE)
} else {
  cat("   No players fell 3+ spots\n")
}

# Summary
cat("\n\n📊 V3 METHODOLOGY SUMMARY:\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("V3 Multipliers Applied:\n")
cat("1. DEFENSIVE VALUE:\n")
cat("   - Catchers: 1.30x (backup C always needed)\n")
cat("   - UTL: 1.20x (roster flexibility)\n")
cat("   - Weak-hitting 1B: 0.80x (corner bat needs elite offense)\n\n")

cat("2. PLATOON SPLITS:\n")
cat("   - MINIMAL splits: 1.10x BONUS (can play everyday)\n")
cat("   - SEVERE splits at platoon-friendly position: 0.95x\n")
cat("   - SEVERE splits at everyday position: 0.85x PENALTY\n\n")

cat("3. INJURY RISK:\n")
cat("   - CRITICAL (surgery, lost season): 0.50x\n")
cat("   - HIGH (significant history): 0.85x\n\n")

cat("Result: Rankings now reflect MLB roster utility, not just raw stats\n\n")
