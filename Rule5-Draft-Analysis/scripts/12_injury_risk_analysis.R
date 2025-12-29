# =============================================================================
# 12_injury_risk_analysis.R
# Identify players with red flag performance drops between levels
# =============================================================================

library(dplyr)
library(tidyr)

# Load cleaned MiLB stats
milb <- read.csv("data/cleaned_milb_stats.csv", stringsAsFactors = FALSE)

cat("Analyzing", nrow(milb), "season records for injury risk indicators\n")

# === Find players with multiple levels in recent seasons ===
# Focus on 2024-2025 promotions (most relevant for injury detection)
recent_promotions <- milb %>%
  filter(season >= 2024) %>%
  group_by(playerid, player_name, season) %>%
  filter(n() > 1) %>%  # Multiple levels in same season
  arrange(playerid, season, level) %>%
  ungroup()

cat("Found", length(unique(recent_promotions$playerid)), "players with multi-level seasons in 2024-2025\n")

# === Calculate performance drops ===
injury_flags <- milb %>%
  arrange(playerid, season, level) %>%
  group_by(playerid, player_name) %>%
  mutate(
    # Get previous level stats
    prev_k9 = lag(k_9),
    prev_bb9 = lag(bb_9),
    prev_era = lag(era),
    prev_fip = lag(fip),
    prev_level = lag(level),
    prev_season = lag(season),
    
    # Calculate % changes
    k9_change_pct = ifelse(!is.na(prev_k9) & prev_k9 > 0, 
                           ((k_9 - prev_k9) / prev_k9) * 100, NA),
    bb9_change_pct = ifelse(!is.na(prev_bb9) & prev_bb9 > 0,
                            ((bb_9 - prev_bb9) / prev_bb9) * 100, NA),
    fip_change_pct = ifelse(!is.na(prev_fip) & prev_fip > 0,
                            ((fip - prev_fip) / prev_fip) * 100, NA),
    
    # Flag concerning patterns
    velocity_concern = k9_change_pct < -25,  # K/9 drops >25%
    command_concern = bb9_change_pct > 75,   # BB/9 increases >75%
    overall_concern = fip_change_pct > 50    # FIP increases >50%
  ) %>%
  filter(
    season >= 2024,  # Recent data only
    (velocity_concern | command_concern | overall_concern)  # At least one red flag
  ) %>%
  select(
    playerid, player_name, season, level, prev_level,
    age, ip, g, gs,
    k_9, prev_k9, k9_change_pct,
    bb_9, prev_bb9, bb9_change_pct,
    fip, prev_fip, fip_change_pct,
    era,
    velocity_concern, command_concern, overall_concern
  ) %>%
  ungroup()

cat("\nIdentified", nrow(injury_flags), "red flag situations\n")

# === Create risk severity score ===
injury_flags <- injury_flags %>%
  mutate(
    # Count number of red flags
    red_flag_count = as.integer(velocity_concern) + 
                     as.integer(command_concern) + 
                     as.integer(overall_concern),
    
    # Severity based on magnitude of changes
    severity_score = abs(k9_change_pct) * 0.4 + 
                     abs(bb9_change_pct) * 0.3 + 
                     abs(fip_change_pct) * 0.3,
    
    risk_level = case_when(
      red_flag_count >= 3 ~ "CRITICAL",
      red_flag_count == 2 ~ "HIGH",
      red_flag_count == 1 ~ "MODERATE",
      TRUE ~ "LOW"
    )
  ) %>%
  arrange(desc(severity_score))

# === Summary by player (may have multiple red flag seasons) ===
player_risk_summary <- injury_flags %>%
  group_by(playerid, player_name) %>%
  summarise(
    total_red_flags = n(),
    max_risk_level = first(risk_level),  # Worst risk level
    latest_season = max(season),
    latest_level = level[which.max(season)],
    worst_k9_drop = min(k9_change_pct, na.rm = TRUE),
    worst_bb9_spike = max(bb9_change_pct, na.rm = TRUE),
    worst_fip_spike = max(fip_change_pct, na.rm = TRUE),
    avg_severity = mean(severity_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_severity))

cat("Summary:", nrow(player_risk_summary), "players with injury risk indicators\n")

# === Export results ===
write.csv(injury_flags, "output/injury_risk_flags.csv", row.names = FALSE)
write.csv(player_risk_summary, "output/injury_risk_summary.csv", row.names = FALSE)

# === Create text report for top risks ===
top_risks <- head(player_risk_summary, 25)

report_lines <- c(
  "=============================================================================",
  "INJURY RISK ANALYSIS - TOP 25 CONCERN PLAYERS",
  "=============================================================================",
  "",
  "Players showing significant performance degradation between levels/seasons",
  "May indicate injury, mechanical issues, or inability to adjust to competition",
  "",
  sprintf("Analysis Date: %s", Sys.Date()),
  sprintf("Total Players Flagged: %d", nrow(player_risk_summary)),
  "",
  "=============================================================================",
  ""
)

for (i in 1:nrow(top_risks)) {
  p <- top_risks[i,]
  
  report_lines <- c(report_lines,
    sprintf("%d. %s", i, p$player_name),
    sprintf("   Risk Level: %s | Severity Score: %.1f", p$max_risk_level, p$avg_severity),
    sprintf("   Latest: %d %s | Red Flags: %d instance(s)", 
            p$latest_season, p$latest_level, p$total_red_flags),
    sprintf("   Worst K/9 Drop: %.1f%% | Worst BB/9 Spike: %.1f%% | Worst FIP Spike: %.1f%%",
            p$worst_k9_drop, p$worst_bb9_spike, p$worst_fip_spike),
    ""
  )
}

writeLines(report_lines, "reports/injury_risk_report.txt")

cat("\n✅ Injury risk analysis complete!\n")
cat("  - Detailed flags: output/injury_risk_flags.csv\n")
cat("  - Player summary: output/injury_risk_summary.csv\n")
cat("  - Text report: reports/injury_risk_report.txt\n")
cat("\nTop 5 Highest Risk:\n")
print(head(player_risk_summary[, c("player_name", "max_risk_level", "avg_severity", 
                                    "worst_k9_drop", "worst_bb9_spike")], 5))
