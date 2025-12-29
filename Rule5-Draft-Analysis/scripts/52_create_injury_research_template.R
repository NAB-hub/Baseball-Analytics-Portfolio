# =============================================================================
# 52_create_injury_research_template.R
# Create injury research template for top hitters (V2 REVISED rankings)
# =============================================================================

library(dplyr)

cat("📋 Creating Injury Research Template for Qualified Hitters (V2)...\n\n")

# Load V2 rankings (strict filters applied)
rankings <- read.csv("output/hitter_rankings_v2.csv", stringsAsFactors = FALSE)

# Select all qualified players (only 13 passed V2 filters)
qualified <- rankings %>%
  select(rank, playerId, player_name, org, latest_position, latest_age, 
         latest_level, recent_wrc, recent_iso, position_value,
         composite_score, role_projection, mlb_ready, flags)

cat(sprintf("✓ Extracted %d qualified hitters\n", nrow(qualified)))

# Create injury research template
# Create injury research template
injury_template <- qualified %>%
  mutate(
    # Empty columns for research
    injury_history = "",
    surgery_type = "",
    recovery_timeline = "",
    current_status = "",
    games_missed_2024 = "",
    games_missed_2025 = "",
    injury_risk = "",  # Will be: CRITICAL, HIGH, MODERATE, NONE
    injury_notes = "",
    research_date = "",
    research_source = "",
    
    # Add warning for Carson Taylor
    special_note = ifelse(player_name == "Carson Taylor", 
                          "⚠️ FAILED 2023 Rule 5 pick - returned to PHI", 
                          "")
  ) %>%
  select(rank, playerId, player_name, org, latest_position, latest_age,
         composite_score, recent_wrc, role_projection, mlb_ready, flags,
         special_note, injury_history, surgery_type, recovery_timeline, 
         current_status, games_missed_2024, games_missed_2025, injury_risk, 
         injury_notes, research_date, research_source)

# Save template
write.csv(injury_template, "config/hitter_injury_research_template.csv", row.names = FALSE)

cat(sprintf("\n✅ Template created: config/hitter_injury_research_template.csv\n"))
cat(sprintf("   - %d qualified players to research\n", nrow(injury_template)))

cat("\n📊 Qualified Hitters by Position:\n")
pos_breakdown <- qualified %>%
  group_by(latest_position) %>%
  summarise(
    count = n(),
    top_player = player_name[which.max(composite_score)],
    .groups = "drop"
  ) %>%
  arrange(desc(count))

for(i in 1:nrow(pos_breakdown)) {
  cat(sprintf("   - %s: %d (Top: %s)\n", 
              pos_breakdown$latest_position[i],
              pos_breakdown$count[i],
              pos_breakdown$top_player[i]))
}

cat("\n⚠️  IMPORTANT NOTES:\n")
cat("   - Only 13 players passed V2 strict filters (95% eliminated)\n")
cat("   - Carson Taylor (#1) FAILED as 2023 Rule 5 pick - SKIP HIM\n")
cat("   - This is a THIN talent pool - proceed with extreme caution\n\n")

cat("\n📋 Research Instructions:\n")
cat("   1. Fill in injury_history for each player\n")
cat("   2. Note any surgeries (TJ, shoulder, ACL, wrist, etc.)\n")
cat("   3. Document recovery status and timeline\n")
cat("   4. Assign injury_risk: CRITICAL/HIGH/MODERATE/NONE\n")
cat("      - CRITICAL: Active injury, recent surgery, recurring issues\n")
cat("      - HIGH: Significant injury history, 2+ DL stints\n")
cat("      - MODERATE: Minor injuries, limited missed time\n")
cat("      - NONE: Clean bill of health\n")
cat("   5. Note Carson Taylor's special_note field\n")
cat("   6. Save completed research to config/hitter_injury_research_complete.csv\n\n")

# Display all qualified players
cat("🎯 All Qualified Rule 5 Hitter Candidates (V2):\n\n")
print(qualified %>% 
        select(rank, player_name, org, latest_position, latest_age, 
               composite_score, role_projection, mlb_ready, flags), 
      row.names = FALSE)

cat("\n")
