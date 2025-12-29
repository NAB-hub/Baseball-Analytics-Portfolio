# =============================================================================
# 40_injury_classification_enhanced.R
# Add injury history, draft classification, and enhanced role assignments
# =============================================================================

library(dplyr)
library(tidyr)

cat("🏥 Building Enhanced Classification System...\n\n")

# Load current data
overall <- read.csv("output/overall_rankings.csv", stringsAsFactors = FALSE)
player_stats <- read.csv("data/player_career_stats.csv", stringsAsFactors = FALSE)

# =============================================================================
# STEP 1: DRAFT CLASSIFICATION
# =============================================================================

cat("📋 Step 1: Draft Classification (MLB Ready / Draft & Stash / Monitor)...\n")

overall <- overall %>%
  mutate(
    # MLB Ready criteria: AAA experience, 24+, strong metrics, solid IP
    mlb_ready_score = case_when(
      latest_level == "AAA" & latest_age >= 24 & career_fip < 4.0 & total_ip >= 100 ~ 3,
      latest_level == "AAA" & latest_age >= 25 & career_fip < 4.5 & total_ip >= 80 ~ 2,
      latest_level == "AA" & latest_age >= 25 & career_fip < 3.5 & total_ip >= 120 ~ 2,
      latest_level == "AAA" & career_fip < 3.5 ~ 2,
      TRUE ~ 0
    ),
    
    # Stash criteria: Young with upside, lower levels but strong stuff
    stash_score = case_when(
      latest_age <= 23 & career_k9 >= 10.5 & latest_level %in% c("A+", "AA") ~ 3,
      latest_age <= 22 & career_k9 >= 9.5 & career_bb9 < 4.0 ~ 2,
      latest_age <= 24 & career_fip < 3.0 & latest_level == "AA" ~ 2,
      TRUE ~ 0
    ),
    
    # Monitor criteria: Concerns but potential
    monitor_score = case_when(
      total_ip < 80 ~ 2,
      career_bb9 > 5.0 ~ 2,
      latest_age >= 28 & latest_level != "AAA" ~ 2,
      career_fip > 4.5 ~ 1,
      TRUE ~ 0
    ),
    
    # Final classification
    draft_classification = case_when(
      mlb_ready_score >= 2 & monitor_score == 0 ~ "MLB Ready",
      mlb_ready_score >= 1 & monitor_score <= 1 ~ "MLB Ready",
      stash_score >= 2 & mlb_ready_score == 0 ~ "Draft & Stash",
      monitor_score >= 2 ~ "Monitor",
      mlb_ready_score == 1 & stash_score >= 1 ~ "Draft & Stash",
      TRUE ~ "Monitor"
    )
  )

# Summary
classification_summary <- overall %>%
  group_by(draft_classification) %>%
  summarise(
    count = n(),
    avg_rank = mean(overall_rank, na.rm = TRUE),
    avg_fip = mean(career_fip, na.rm = TRUE)
  )

cat("\n📊 Draft Classification Summary:\n")
print(classification_summary)

# =============================================================================
# STEP 2: ENHANCED ROLE CLASSIFICATION
# =============================================================================

cat("\n🎯 Step 2: Enhanced Role Classification...\n")

overall <- overall %>%
  mutate(
    # Detailed role based on usage patterns, stuff, and profile
    enhanced_role = case_when(
      
      # High-Leverage Closer Types
      career_k9 >= 11 & career_bb9 < 3.5 & avg_ip_per_game <= 1.2 & 
        total_saves >= 5 ~ "High-Leverage Closer",
      
      # Setup/High-Leverage (power arms)
      career_k9 >= 10.5 & career_bb9 < 4.0 & avg_ip_per_game <= 1.5 ~ "Setup / High-Leverage",
      
      # Multi-Inning / Swing (can go 2+ innings)
      avg_ip_per_game >= 1.8 & starter_percentage < 30 & career_fip < 4.0 ~ "Multi-Inning Reliever",
      
      # Groundball Specialist
      !is.na(career_gb_pct) & career_gb_pct >= 50 & career_bb9 < 3.5 ~ "Groundball Specialist",
      
      # LHP Specialist (matchup reliever)
      pitcher_hand == "L" & avg_ip_per_game < 1.2 & career_k9 >= 8.5 ~ "LHP Matchup Specialist",
      
      # Versatile swingman (can start or relieve)
      starter_percentage >= 30 & starter_percentage <= 70 & 
        avg_ip_per_game >= 2.0 ~ "Swingman (SP/RP)",
      
      # Middle reliever (bulk innings, mop-up)
      avg_ip_per_game >= 1.5 & career_k9 < 9.5 ~ "Middle Reliever",
      
      # General reliever
      role == "Reliever" ~ "Standard Reliever",
      
      # Starter (not primary focus)
      starter_percentage >= 50 ~ "Starter (Not RP focus)",
      
      TRUE ~ "Unclassified"
    ),
    
    # Role appeal (how valuable is this role for Rule 5?)
    role_appeal = case_when(
      enhanced_role %in% c("High-Leverage Closer", "Setup / High-Leverage") ~ "High",
      enhanced_role %in% c("Multi-Inning Reliever", "LHP Matchup Specialist", "Groundball Specialist") ~ "Medium-High",
      enhanced_role %in% c("Swingman (SP/RP)", "Standard Reliever") ~ "Medium",
      enhanced_role == "Middle Reliever" ~ "Low-Medium",
      TRUE ~ "Low"
    )
  )

# Role summary
role_summary <- overall %>%
  group_by(enhanced_role, role_appeal) %>%
  summarise(
    count = n(),
    avg_rank = mean(overall_rank, na.rm = TRUE),
    avg_k9 = mean(career_k9, na.rm = TRUE),
    avg_fip = mean(career_fip, na.rm = TRUE)
  ) %>%
  arrange(role_appeal, avg_rank)

cat("\n📋 Enhanced Role Summary:\n")
print(role_summary, n = 30)

# =============================================================================
# STEP 3: INJURY HISTORY PLACEHOLDER
# =============================================================================

cat("\n🏥 Step 3: Injury History (Manual Entry Required)...\n")

# Create injury tracking columns
overall <- overall %>%
  mutate(
    # Placeholder - to be filled manually
    injury_history = NA_character_,
    injury_risk = "Unknown",
    injury_notes = NA_character_,
    
    # Flag for known injuries (to be updated manually)
    has_known_injury = FALSE
  )

# Create injury template for manual entry
injury_template <- overall %>%
  filter(overall_rank <= 50) %>%
  select(overall_rank, player_name, playerid, latest_age, total_ip, 
         draft_classification, enhanced_role) %>%
  mutate(
    injury_history = "",
    injury_date = "",
    injury_type = "",
    injury_severity = "",  # Minor / Moderate / Major / Career-Threatening
    games_missed = "",
    injury_notes = ""
  )

write.csv(injury_template, "output/injury_research_template.csv", row.names = FALSE)

cat("\n📝 Created injury research template: output/injury_research_template.csv\n")
cat("   → Manual research needed for top 50 players\n")
cat("   → Fill in injury details from FanGraphs, MiLB news, team reports\n")
cat("   → Re-import and merge with main dataset\n\n")

# =============================================================================
# STEP 4: SAVE ENHANCED DATA
# =============================================================================

cat("💾 Saving enhanced classifications...\n")

write.csv(overall, "output/overall_rankings_enhanced.csv", row.names = FALSE)

cat("\n✅ ENHANCED CLASSIFICATION COMPLETE!\n\n")
cat("📊 Summary:\n")
cat("   - Draft Classifications added:", nrow(classification_summary), "categories\n")
cat("   - Enhanced Roles added:", nrow(role_summary), "role types\n")
cat("   - Injury template created for manual research\n\n")

cat("📋 Next Steps:\n")
cat("   1. Review output/injury_research_template.csv\n")
cat("   2. Research injury history for top prospects (FanGraphs, Baseball America, team reports)\n")
cat("   3. Fill in injury data and severity ratings\n")
cat("   4. Run script 41 to merge injury data and regenerate HTML report\n\n")

# =============================================================================
# BONUS: Top Lists by Role
# =============================================================================

cat("🎯 Generating Role-Specific Top Lists...\n\n")

# High-Leverage RHP
high_leverage_rhp <- overall %>%
  filter(enhanced_role %in% c("High-Leverage Closer", "Setup / High-Leverage"),
         pitcher_hand == "R") %>%
  arrange(overall_rank) %>%
  head(10)

cat("🔥 Top 10 High-Leverage RHP:\n")
print(high_leverage_rhp %>% select(overall_rank, player_name, latest_age, latest_level, 
                                    career_fip, career_k9, enhanced_role, draft_classification))

# High-Leverage LHP
high_leverage_lhp <- overall %>%
  filter(enhanced_role %in% c("High-Leverage Closer", "Setup / High-Leverage", "LHP Matchup Specialist"),
         pitcher_hand == "L") %>%
  arrange(overall_rank) %>%
  head(10)

cat("\n🔥 Top 10 High-Leverage LHP:\n")
print(high_leverage_lhp %>% select(overall_rank, player_name, latest_age, latest_level, 
                                    career_fip, career_k9, enhanced_role, draft_classification))

# Multi-Inning Relievers
multi_inning <- overall %>%
  filter(enhanced_role == "Multi-Inning Reliever") %>%
  arrange(overall_rank) %>%
  head(10)

cat("\n📏 Top 10 Multi-Inning Relievers:\n")
print(multi_inning %>% select(overall_rank, player_name, latest_age, latest_level, 
                               career_fip, avg_ip_per_game, enhanced_role, draft_classification))

# Save role-specific lists
write.csv(high_leverage_rhp, "output/top10_high_leverage_rhp.csv", row.names = FALSE)
write.csv(high_leverage_lhp, "output/top10_high_leverage_lhp.csv", row.names = FALSE)
write.csv(multi_inning, "output/top10_multi_inning_relievers.csv", row.names = FALSE)

cat("\n✅ Role-specific lists saved to output/\n")
