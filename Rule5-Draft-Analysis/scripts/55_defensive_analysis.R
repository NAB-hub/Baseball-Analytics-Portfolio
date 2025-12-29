# =============================================================================
# 55_defensive_analysis.R
# Defensive Value Assessment for Rule 5 Hitter Candidates
# 
# Since we don't have defensive metrics, we'll use proxies:
# 1. Position played (premium vs corner)
# 2. Speed score (athleticism proxy)
# 3. Multi-position ability (UTL tag)
# 4. Organizational trust (did they keep them at premium positions?)
# =============================================================================

library(dplyr)

cat("🛡️  Defensive Value Analysis for Rule 5 Candidates\n\n")

# Load data
rankings <- read.csv("output/hitter_rankings_v2.csv", stringsAsFactors = FALSE)
seasons <- read.csv("data/hitters_cleaned_seasons.csv", stringsAsFactors = FALSE)

# Get position history for each player
position_history <- seasons %>%
  group_by(playerId, player_name) %>%
  summarise(
    positions_played = paste(unique(position_bucket), collapse = ", "),
    num_positions = n_distinct(position_bucket),
    latest_position = last(position_bucket),
    has_premium_history = any(position_bucket %in% c("C", "SS", "CF", "2B")),
    .groups = "drop"
  )

# Merge with rankings
defensive_analysis <- rankings %>%
  filter(rank <= 15) %>%
  left_join(position_history, by = c("playerId", "player_name")) %>%
  mutate(
    # Defensive value tiers
    defensive_tier = case_when(
      latest_position.x == "C" ~ "ELITE - Catcher (scarce)",
      latest_position.x == "SS" ~ "PREMIUM - Up middle",
      latest_position.x == "CF" ~ "PREMIUM - Up middle",
      latest_position.x == "2B" ~ "ABOVE AVG - Middle IF",
      latest_position.x %in% c("3B", "UTL") ~ "AVERAGE - Versatile",
      latest_position.x == "OF" ~ "AVERAGE - Corner OF",
      latest_position.x == "1B" ~ "LOW - Corner only",
      TRUE ~ "UNKNOWN"
    ),
    
    # Versatility score (can they play multiple positions?)
    versatility_score = case_when(
      latest_position.x == "UTL" ~ 10,  # True utility
      num_positions >= 3 ~ 8,          # Multi-position
      num_positions == 2 ~ 5,          # Some versatility
      TRUE ~ 2                         # One position only
    ),
    
    # Rule 5 defensive value (can they help MLB roster NOW?)
    r5_defensive_value = case_when(
      latest_position.x == "C" ~ "HIGH - Backup C always needed",
      latest_position.x == "UTL" ~ "HIGH - Super-utility bench value",
      latest_position.x %in% c("SS", "CF", "2B") & has_premium_history ~ "MEDIUM - Can spell starters",
      latest_position.x %in% c("3B", "OF") ~ "MEDIUM - Role player depth",
      latest_position.x == "1B" ~ "LOW - Needs elite bat to justify",
      TRUE ~ "UNKNOWN"
    )
  ) %>%
  select(rank, player_name, org, latest_position = latest_position.x, positions_played, num_positions,
         defensive_tier, versatility_score, r5_defensive_value, 
         recent_wrc, composite_score, mlb_ready)

cat("🎯 Top 15 Defensive Value Assessment:\n\n")
print(defensive_analysis, row.names = FALSE)

# Red flags
cat("\n\n🚨 DEFENSIVE RED FLAGS:\n\n")

red_flags <- defensive_analysis %>%
  filter(
    (latest_position == "1B" & recent_wrc < 130) |  # Weak-hitting 1B
    (num_positions == 1 & !latest_position %in% c("C", "SS", "CF"))  # One-position corner player
  ) %>%
  mutate(
    flag_reason = case_when(
      latest_position == "1B" & recent_wrc < 130 ~ "Weak bat for 1B-only player",
      num_positions == 1 & latest_position == "3B" ~ "3B-only with no versatility",
      num_positions == 1 & latest_position == "OF" ~ "OF-only with no positional value",
      TRUE ~ "Limited defensive value"
    )
  ) %>%
  select(rank, player_name, latest_position, recent_wrc, flag_reason)

if (nrow(red_flags) > 0) {
  print(red_flags, row.names = FALSE)
} else {
  cat("   ✓ No major defensive red flags in top 15\n")
}

# Defensive value winners
cat("\n\n✅ DEFENSIVE VALUE WINNERS:\n\n")

winners <- defensive_analysis %>%
  filter(
    defensive_tier %in% c("ELITE - Catcher (scarce)", "PREMIUM - Up middle") |
    versatility_score >= 8
  ) %>%
  arrange(desc(versatility_score)) %>%
  select(rank, player_name, latest_position, defensive_tier, versatility_score, r5_defensive_value)

if (nrow(winners) > 0) {
  print(winners, row.names = FALSE)
} else {
  cat("   ⚠️  No premium defensive players in top 15\n")
}

# Summary by position
cat("\n\n📊 Position Breakdown (Top 15):\n\n")

position_summary <- defensive_analysis %>%
  group_by(defensive_tier) %>%
  summarise(
    count = n(),
    avg_wrc = round(mean(recent_wrc), 1),
    players = paste(player_name, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

print(position_summary, row.names = FALSE)

cat("\n\n💡 KEY INSIGHTS:\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("For Rule 5 Draft:\n")
cat("1. CATCHERS = Premium value (backup C is always needed)\n")
cat("2. UTL players = High value (can cover injuries, platoons)\n")
cat("3. Premium positions (SS/CF/2B) = Medium value if MLB-ready defensively\n")
cat("4. Corner-only (1B/LF/RF) = MUST have elite bat to justify roster spot\n\n")

cat("⚠️  Without UZR/DRS/OAA data, we're assuming organizational trust:\n")
cat("   - If they kept him at SS/CF/C, likely competent there\n")
cat("   - If moved to 1B/LF, likely defensive concerns\n")
cat("   - UTL tag = either versatile or 'tweener'\n\n")

cat("🔍 RESEARCH PRIORITY:\n")
cat("For top candidates, manually check:\n")
cat("- Scouting reports mentioning defense/arm/range\n")
cat("- Position changes (moved OFF premium position = red flag)\n")
cat("- Organizational depth charts (blocked or starter track?)\n\n")

# Save
write.csv(defensive_analysis, "output/defensive_value_analysis.csv", row.names = FALSE)
cat("✅ Saved to: output/defensive_value_analysis.csv\n\n")
