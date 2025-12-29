# =============================================================================
# 19_mlb_roster_projection.R
# MLB Roster Worthiness - Can They Stick on a 26-Man Roster?
# Critical for Rule 5 Draft: Must contribute immediately or be returned
# =============================================================================

library(dplyr)

# Load composite rankings
player_stats <- read.csv("data/player_stats_composite.csv", stringsAsFactors = FALSE)

cat("Evaluating MLB roster worthiness for", nrow(player_stats), "pitchers...\n")
cat("Rule 5 Draft requirement: Must stick on 26-man roster or be offered back\n\n")

# === MLB Roster Projection Framework ===
# Based on historical Rule 5 success criteria:
# 1. Immediate Role Definition (30%)
# 2. MLB-Ready Stuff (25%)
# 3. Command/Control (20%)
# 4. Experience Level (15%)
# 5. Durability/Health (10%)

player_stats <- player_stats %>%
  mutate(
    # 1. ROLE DEFINITION SCORE
    # Can we define a clear MLB role right now?
    role_definition_score = case_when(
      # LOOGY/Specialist (LHP with platoon advantage)
      pitcher_hand == "L" & career_k9 >= 8.5 ~ 95,  # LHP with strikeouts = instant value
      
      # Multi-inning swing man (IP/G > 1.5, low HR/9)
      career_ip_per_game >= 1.5 & career_hr_9 < 0.8 & career_fip < 4.0 ~ 100,
      
      # 7th/8th inning setup (elite K/9, good control)
      career_k9 >= 10.5 & career_bb9 < 3.5 & career_fip < 3.5 ~ 100,
      
      # Ground ball specialist (GB% high, can eat innings)
      career_gb_pct >= 50 & career_bb9 < 3.5 ~ 90,
      
      # One-inning specialist (strikeouts, might allow solo HR)
      career_k9 >= 11.0 ~ 85,
      
      # Solid middle reliever
      career_k9 >= 9.0 & career_bb9 < 4.0 ~ 75,
      
      # Questionable role fit
      TRUE ~ 50
    ),
    
    # 2. MLB-READY STUFF (using Stuff+ proxy from Tier 3)
    mlb_stuff_score = case_when(
      stuff_plus >= 115 ~ 100,  # Elite stuff, can get MLB hitters out now
      stuff_plus >= 110 ~ 95,   # Plus-plus, very MLB ready
      stuff_plus >= 105 ~ 90,   # Plus stuff, should handle MLB
      stuff_plus >= 100 ~ 80,   # Average stuff, needs other skills
      stuff_plus >= 95 ~ 65,    # Below average, risky
      TRUE ~ 50                 # Likely not MLB ready
    ),
    
    # 3. COMMAND & CONTROL (BB/9 critical - can't walk MLB hitters)
    command_score = case_when(
      career_bb9 <= 2.0 ~ 100,  # Elite command
      career_bb9 <= 2.5 ~ 95,   # Plus command
      career_bb9 <= 3.0 ~ 85,   # Above average
      career_bb9 <= 3.5 ~ 75,   # Average (acceptable for relievers)
      career_bb9 <= 4.0 ~ 60,   # Below average (risky)
      career_bb9 <= 5.0 ~ 45,   # Poor command (major concern)
      TRUE ~ 30                 # Too many walks, won't stick
    ),
    
    # 4. EXPERIENCE LEVEL (AAA time critical for MLB readiness)
    experience_score = case_when(
      # AAA experience is gold standard
      latest_level == "AAA" & total_seasons >= 3 ~ 100,  # Veteran AAA
      latest_level == "AAA" & total_seasons >= 2 ~ 95,   # Solid AAA time
      latest_level == "AAA" ~ 85,                        # Some AAA
      
      # AA can work if dominant
      latest_level == "AA" & career_fip < 3.0 & career_k9 > 10 ~ 80,  # AA star
      latest_level == "AA" & latest_age <= 23 ~ 75,                    # Young AA
      latest_level == "AA" ~ 65,                                       # AA experience
      
      # A+ is risky for Rule 5
      latest_level == "A+" & career_fip < 2.5 ~ 60,  # A+ domination
      latest_level == "A+" ~ 40,                     # Likely not ready
      
      TRUE ~ 30
    ),
    
    # 5. DURABILITY & AVAILABILITY (injuries/IP totals)
    durability_score = case_when(
      # High IP total, recent full season
      total_ip >= 150 & latest_season >= 2024 & latest_ip >= 40 ~ 100,
      total_ip >= 100 & latest_season >= 2024 ~ 90,
      total_ip >= 80 & latest_season >= 2024 ~ 80,
      total_ip >= 60 ~ 70,
      total_ip >= 40 ~ 60,
      TRUE ~ 50
    ),
    
    # ADDITIONAL FACTORS
    
    # Age factor (Rule 5 picks typically 23-26)
    age_factor = case_when(
      latest_age <= 24 ~ 1.05,  # Young upside (5% bonus)
      latest_age <= 26 ~ 1.00,  # Prime age
      latest_age <= 28 ~ 0.95,  # Older, less upside
      TRUE ~ 0.90               # Veteran (might have flaws)
    ),
    
    # Track record consistency (seasons with FIP < 4.0)
    consistency_bonus = case_when(
      best_fip < 3.0 & career_fip < 3.5 ~ 5,  # Consistently excellent
      best_fip < 3.5 & career_fip < 4.0 ~ 3,  # Solid track record
      TRUE ~ 0
    ),
    
    # === MLB ROSTER WORTHINESS SCORE ===
    mlb_roster_score = (
      (role_definition_score * 0.30 +
       mlb_stuff_score * 0.25 +
       command_score * 0.20 +
       experience_score * 0.15 +
       durability_score * 0.10) * age_factor
    ) + consistency_bonus,
    
    # Cap at 100
    mlb_roster_score = pmin(100, mlb_roster_score),
    
    # MLB Readiness Tier
    mlb_readiness = case_when(
      mlb_roster_score >= 85 ~ "MLB Ready - High Confidence",
      mlb_roster_score >= 75 ~ "MLB Ready - Good Bet",
      mlb_roster_score >= 65 ~ "MLB Capable - Risky",
      mlb_roster_score >= 55 ~ "Needs Seasoning",
      TRUE ~ "Not MLB Ready"
    ),
    
    # Stick Probability (can they stay on roster all year?)
    stick_probability = case_when(
      mlb_roster_score >= 85 & command_score >= 80 ~ "Very High (75%+)",
      mlb_roster_score >= 80 & command_score >= 70 ~ "High (60-75%)",
      mlb_roster_score >= 70 ~ "Moderate (40-60%)",
      mlb_roster_score >= 60 ~ "Low (25-40%)",
      TRUE ~ "Very Low (<25%)"
    ),
    
    # Defined MLB Role
    projected_mlb_role = case_when(
      pitcher_hand == "L" & career_k9 >= 9.0 ~ "LOOGY/Matchup Specialist",
      career_ip_per_game >= 1.5 & career_fip < 4.0 ~ "Multi-Inning Swing Man",
      career_k9 >= 11.0 & career_bb9 < 3.0 ~ "Setup Man (7th/8th)",
      career_k9 >= 10.0 & career_fip < 3.5 ~ "Middle Reliever (6th/7th)",
      career_gb_pct >= 50 & career_bb9 < 3.5 ~ "Ground Ball Specialist",
      career_k9 >= 9.0 ~ "Middle Relief",
      TRUE ~ "Long Relief/Mop-Up"
    )
  )

# === Summary ===

cat("\n=== MLB Roster Projection Summary ===\n")
cat("Mean MLB Roster Score:", round(mean(player_stats$mlb_roster_score, na.rm = TRUE), 1), "\n")
cat("Median MLB Roster Score:", round(median(player_stats$mlb_roster_score, na.rm = TRUE), 1), "\n\n")

cat("MLB Readiness Distribution:\n")
print(table(player_stats$mlb_readiness))

cat("\n\nStick Probability Distribution:\n")
print(table(player_stats$stick_probability))

cat("\n\nProjected MLB Role Distribution:\n")
print(table(player_stats$projected_mlb_role))

cat("\n\nTop 15 MLB-Ready Pitchers (Roster Worthiness):\n")
top15_mlb <- player_stats %>%
  arrange(desc(mlb_roster_score)) %>%
  select(player_name, mlb_roster_score, mlb_readiness, stick_probability, 
         projected_mlb_role, latest_level, latest_age, stuff_plus, career_bb9) %>%
  head(15)
print(as.data.frame(top15_mlb), row.names = FALSE)

cat("\n\nRule 5 Draft Targets (MLB Ready + High Stick Probability):\n")
rule5_targets <- player_stats %>%
  filter(mlb_roster_score >= 75 & command_score >= 70) %>%
  arrange(desc(mlb_roster_score)) %>%
  select(player_name, mlb_roster_score, stick_probability, projected_mlb_role,
         career_fip, career_k9, career_bb9, latest_level, latest_age)

if (nrow(rule5_targets) > 0) {
  print(as.data.frame(rule5_targets), row.names = FALSE)
  cat(sprintf("\nIdentified %d legitimate Rule 5 Draft targets\n", nrow(rule5_targets)))
} else {
  cat("No players meet strict Rule 5 criteria in current dataset\n")
}

# === Save Results ===

write.csv(player_stats, "data/player_stats_mlb_projected.csv", row.names = FALSE)

# Save Rule 5 specific target list
if (nrow(rule5_targets) > 0) {
  write.csv(rule5_targets, "output/rule5_mlb_ready_targets.csv", row.names = FALSE)
  cat("\n✅ Rule 5 targets saved to: output/rule5_mlb_ready_targets.csv\n")
}

cat("\n✅ MLB roster projection complete!\n")
cat("  - Results saved to: data/player_stats_mlb_projected.csv\n")
cat("  - Focus: Immediate MLB roster worthiness (can they stick?)\n")
