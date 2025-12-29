# =============================================================================
# 17_scouting_reports.R
# Generate detailed scouting reports for Top 20 Rule 5 targets
# =============================================================================

library(dplyr)
library(stringr)

# Load top 25 with profiles
top25 <- read.csv("output/top25_with_profiles.csv", stringsAsFactors = FALSE)

cat("Generating scouting reports for", nrow(top25), "prospects...\n")

# === Helper Functions ===

get_strength_summary <- function(tier1, tier2, tier3) {
  strengths <- c()
  
  if (tier1 >= 75) strengths <- c(strengths, "elite fundamentals")
  if (tier2 >= 75) strengths <- c(strengths, "strong advanced metrics")
  if (tier3 >= 75) strengths <- c(strengths, "high MLB readiness")
  
  if (length(strengths) == 0) {
    strengths <- "solid all-around profile"
  } else {
    strengths <- paste(strengths, collapse = ", ")
  }
  
  return(strengths)
}

get_concerns <- function(tier1, tier2, tier3, bb9, age, level) {
  concerns <- c()
  
  if (bb9 > 4.0) concerns <- c(concerns, "control issues")
  if (age > 26 & level != "AAA") concerns <- c(concerns, "older for level")
  if (tier1 < 60) concerns <- c(concerns, "foundational metrics need improvement")
  if (tier3 < 50) concerns <- c(concerns, "projection risk")
  
  if (length(concerns) == 0) {
    return("No major red flags")
  } else {
    return(paste(concerns, collapse = ", "))
  }
}

get_role_projection <- function(role, ip_per_g, k9, fip) {
  if (role == "Multi-Inning") {
    if (fip < 3.2 & k9 > 9.5) {
      return("High-leverage multi-inning weapon, can handle 2-3 innings regularly")
    } else if (ip_per_g > 1.5) {
      return("Swing man/long reliever, capable of bridging multiple innings")
    } else {
      return("Versatile reliever with multi-inning capability")
    }
  } else {  # One-Inning
    if (k9 > 11 & fip < 3.0) {
      return("Back-end closer material with elite swing-and-miss stuff")
    } else if (k9 > 10) {
      return("High-leverage setup option, potential closer")
    } else {
      return("Middle-to-late inning specialist")
    }
  }
}

get_acquisition_recommendation <- function(rank, composite, grade, role) {
  priority <- case_when(
    rank <= 5 ~ "HIGHEST PRIORITY",
    rank <= 10 ~ "HIGH PRIORITY",
    rank <= 15 ~ "MEDIUM PRIORITY",
    TRUE ~ "CONSIDERATION"
  )
  
  if (composite >= 75) {
    rec <- "Strong Rule 5 Draft selection. High probability of MLB contribution."
  } else if (composite >= 65) {
    rec <- "Worthwhile gamble in Rule 5 Draft. Good upside with manageable risk."
  } else {
    rec <- "Solid depth option if available. Consider organizational fit."
  }
  
  return(paste0(priority, " - ", rec))
}

# === Generate Reports ===

reports <- list()

for (i in 1:nrow(top25)) {
  player <- top25[i, ]
  
  report <- sprintf("
================================================================================
RULE 5 DRAFT SCOUTING REPORT
================================================================================

PLAYER: %s
RANK: #%d Overall | %s Pitcher
AGE: %s | LEVEL: %s
ORGANIZATION: [From MiLB Data]

--------------------------------------------------------------------------------
OVERALL GRADE: %s (%.1f/100)
--------------------------------------------------------------------------------

COMPOSITE SCORES:
  • Tier 1 (Fundamentals - 50%%): %s (%.1f/100)
  • Tier 2 (Advanced Metrics - 30%%): %s (%.1f/100)  
  • Tier 3 (Projection - 20%%): %s (%.1f/100)

--------------------------------------------------------------------------------
STATISTICAL PROFILE
--------------------------------------------------------------------------------

Career MiLB Performance (A+ and above):
  • ERA: %.2f | FIP: %.2f | WHIP: %.2f
  • K/9: %.1f | BB/9: %.1f | K/BB: %.2f
  • Ground Ball %%: %.1f%%
  • Innings Pitched: %.1f | Games: %d | Starts: %d

Usage Pattern:
  • IP/Game: %.1f | Starter %%: %.1f%%
  • Classification: %s

--------------------------------------------------------------------------------
PITCHER PROFILE
--------------------------------------------------------------------------------

Primary Profile: %s
Archetype: %s
Key Traits: %s

--------------------------------------------------------------------------------
SCOUTING SUMMARY
--------------------------------------------------------------------------------

STRENGTHS:
%s

AREAS OF CONCERN:
%s

MLB ROLE PROJECTION:
%s

--------------------------------------------------------------------------------
ACQUISITION RECOMMENDATION
--------------------------------------------------------------------------------

%s

RULE 5 ROSTER FIT:
Can be rostered on MLB 26-man roster for entire season? %s
Risk Level: %s

--------------------------------------------------------------------------------
NOTES:
• All statistics from A+/AA/AAA levels (2019-2025)
• Weighted toward recent performance and higher competition levels
• Comparables based on statistical similarity to current MLB relievers

================================================================================
",
    # Header info
    player$player_name,
    player$overall_rank,
    player$primary_role,
    player$latest_age,
    player$latest_level,
    
    # Overall grade
    player$overall_grade,
    player$composite_score,
    
    # Tier scores
    player$tier1_grade, player$tier1_score,
    player$tier2_grade, player$tier2_score,
    player$tier3_grade, player$tier3_score,
    
    # Stats
    player$career_era, player$career_fip, player$career_whip,
    player$career_k9, player$career_bb9, (player$career_k9 / pmax(player$career_bb9, 0.1)),
    player$career_gb_pct,
    player$total_ip, player$total_games, player$total_starts,
    
    # Usage
    player$avg_ip_per_game, player$starter_percentage,
    player$pitcher_type,
    
    # Profile
    player$primary_profile,
    player$archetype,
    player$secondary_traits,
    
    # Scouting narrative
    get_strength_summary(player$tier1_score, player$tier2_score, player$tier3_score),
    get_concerns(player$tier1_score, player$tier2_score, player$tier3_score, 
                 player$career_bb9, player$latest_age, player$latest_level),
    get_role_projection(player$primary_role, player$avg_ip_per_game, 
                       player$career_k9, player$career_fip),
    
    # Recommendation
    get_acquisition_recommendation(player$overall_rank, player$composite_score, 
                                   player$overall_grade, player$primary_role),
    
    # Rule 5 fit
    ifelse(player$composite_score >= 65, "YES - High confidence", 
           ifelse(player$composite_score >= 55, "POSSIBLY - With development", "RISKY")),
    ifelse(player$composite_score >= 70, "LOW", 
           ifelse(player$composite_score >= 60, "MODERATE", "MODERATE-HIGH"))
  )
  
  reports[[i]] <- report
  
  # Save individual report
  filename <- paste0("reports/scouting_report_", 
                    sprintf("%02d", player$overall_rank), "_",
                    gsub(" ", "_", player$player_name), ".txt")
  writeLines(report, filename)
}

# Create master report with all 25
master_report <- paste(reports, collapse = "\n\n\n")
writeLines(master_report, "reports/TOP25_MASTER_SCOUTING_REPORT.txt")

# Create summary table
summary_table <- top25 %>%
  select(overall_rank, player_name, primary_role, overall_grade, composite_score,
         latest_age, latest_level, career_era, career_k9, career_bb9,
         primary_profile, archetype)

# Add recommendations
summary_table$recommendation <- sapply(1:nrow(summary_table), function(i) {
  get_acquisition_recommendation(
    summary_table$overall_rank[i], 
    summary_table$composite_score[i], 
    summary_table$overall_grade[i], 
    summary_table$primary_role[i]
  )
})

write.csv(summary_table, "reports/top25_summary_table.csv", row.names = FALSE)

cat("\n✅ Scouting reports generated!\n")
cat("  - Individual reports: reports/scouting_report_*.txt\n")
cat("  - Master report: reports/TOP25_MASTER_SCOUTING_REPORT.txt\n")
cat("  - Summary table: reports/top25_summary_table.csv\n")
