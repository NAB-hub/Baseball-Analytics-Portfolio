# =============================================================================
# 41_merge_injury_data.R
# Merge injury research into rankings and add risk classifications
# =============================================================================

library(dplyr)

cat("🏥 Merging Injury Data into Rankings...\n\n")

# Load data
overall <- read.csv("output/overall_rankings.csv", stringsAsFactors = FALSE)
injury_data <- read.csv("output/injury_research_complete.csv", stringsAsFactors = FALSE)

cat(sprintf("✓ Loaded %d ranked pitchers\n", nrow(overall)))
cat(sprintf("✓ Loaded %d injury records\n", nrow(injury_data)))

# Clean up injury data
injury_data <- injury_data %>%
  select(PlayerID, Injury.History, IL.Stints..Dates., Injury.Type, Severity) %>%
  rename(
    playerid = PlayerID,
    injury_history = Injury.History,
    il_stints = IL.Stints..Dates.,
    injury_type = Injury.Type,
    injury_severity = Severity
  ) %>%
  mutate(
    has_injury_history = !is.na(injury_history) & injury_history != "",
    
    # Classify injury risk
    injury_risk = case_when(
      # CRITICAL: Tommy John, season-ending surgery
      grepl("Tommy John|season-ending|UCL tear", injury_history, ignore.case = TRUE) ~ "CRITICAL",
      grepl("Tommy John|season-ending|UCL", injury_type, ignore.case = TRUE) ~ "CRITICAL",
      
      # HIGH: Multiple IL stints, elbow issues, stress fractures
      grepl("elbow.*surgery|stress fracture|60-day", injury_history, ignore.case = TRUE) ~ "HIGH",
      grepl("multiple|5/6/2025.*5/21/2025", il_stints, ignore.case = TRUE) ~ "HIGH",
      
      # MODERATE: Single IL stint, shoulder/arm strains
      grepl("7-day IL|shoulder|arm strain", injury_history, ignore.case = TRUE) ~ "MODERATE",
      injury_severity == "Moderate" ~ "MODERATE",
      
      # No documented injury
      !has_injury_history ~ "NONE",
      
      TRUE ~ "UNKNOWN"
    ),
    
    # Flag for Tommy John specifically
    tommy_john_history = grepl("Tommy John|UCL tear|UCL reconstruction", 
                                paste(injury_history, injury_type), ignore.case = TRUE),
    
    # Injury notes for display
    injury_summary = case_when(
      injury_risk == "CRITICAL" ~ paste0("⚠️ CRITICAL: ", 
                                         substr(injury_history, 1, 60)),
      injury_risk == "HIGH" ~ paste0("⚠️ HIGH RISK: ", 
                                     substr(injury_history, 1, 60)),
      injury_risk == "MODERATE" ~ paste0("⚠️ ", substr(injury_history, 1, 50)),
      TRUE ~ ""
    )
  )

# Merge with overall rankings
overall_with_injuries <- overall %>%
  left_join(
    injury_data %>% select(playerid, has_injury_history, injury_risk, tommy_john_history, 
                          injury_summary, injury_history, il_stints, injury_type),
    by = "playerid"
  ) %>%
  mutate(
    # Fill in NONE for players without injury data
    injury_risk = ifelse(is.na(injury_risk), "NONE", injury_risk),
    has_injury_history = ifelse(is.na(has_injury_history), FALSE, has_injury_history),
    tommy_john_history = ifelse(is.na(tommy_john_history), FALSE, tommy_john_history),
    injury_summary = ifelse(is.na(injury_summary), "", injury_summary)
  )

# Summary statistics
cat("\n📊 INJURY RISK SUMMARY:\n")
injury_summary_stats <- overall_with_injuries %>%
  group_by(injury_risk) %>%
  summarise(
    count = n(),
    avg_rank = mean(overall_rank, na.rm = TRUE),
    top25_count = sum(overall_rank <= 25)
  ) %>%
  arrange(match(injury_risk, c("CRITICAL", "HIGH", "MODERATE", "NONE", "UNKNOWN")))

print(injury_summary_stats)

# Top 25 injury breakdown
cat("\n🎯 TOP 25 INJURY BREAKDOWN:\n")
top25_injuries <- overall_with_injuries %>%
  filter(overall_rank <= 25) %>%
  group_by(injury_risk) %>%
  summarise(count = n()) %>%
  arrange(match(injury_risk, c("CRITICAL", "HIGH", "MODERATE", "NONE")))

print(top25_injuries)

# Critical players list
cat("\n⚠️  CRITICAL INJURY PLAYERS (Top 50):\n")
critical_players <- overall_with_injuries %>%
  filter(injury_risk == "CRITICAL", overall_rank <= 50) %>%
  select(overall_rank, player_name, latest_age, latest_level, overall_grade, injury_summary) %>%
  arrange(overall_rank)

if(nrow(critical_players) > 0) {
  print(critical_players)
} else {
  cat("   None in top 50\n")
}

# High risk players list
cat("\n⚠️  HIGH RISK INJURY PLAYERS (Top 50):\n")
high_risk_players <- overall_with_injuries %>%
  filter(injury_risk == "HIGH", overall_rank <= 50) %>%
  select(overall_rank, player_name, latest_age, latest_level, overall_grade, injury_summary) %>%
  arrange(overall_rank)

if(nrow(high_risk_players) > 0) {
  print(high_risk_players)
} else {
  cat("   None in top 50\n")
}

# Save enhanced rankings
write.csv(overall_with_injuries, "output/overall_rankings_with_injuries.csv", row.names = FALSE)

# Create injury risk summary report
injury_risk_report <- overall_with_injuries %>%
  filter(has_injury_history) %>%
  select(overall_rank, player_name, playerid, injury_risk, tommy_john_history, 
         injury_summary, latest_age, latest_level, composite_score, overall_grade) %>%
  arrange(injury_risk, overall_rank)

write.csv(injury_risk_report, "output/injury_risk_report.csv", row.names = FALSE)

cat("\n✅ INJURY DATA MERGED SUCCESSFULLY!\n\n")
cat("📁 Files created:\n")
cat("   - output/overall_rankings_with_injuries.csv (all 527 pitchers + injury flags)\n")
cat("   - output/injury_risk_report.csv (players with documented injuries)\n\n")

cat("📋 Next Steps:\n")
cat("   1. Review critical/high risk players above\n")
cat("   2. Run script 42 to regenerate HTML with injury warnings\n")
cat("   3. Consider removing CRITICAL players from primary targets\n\n")
