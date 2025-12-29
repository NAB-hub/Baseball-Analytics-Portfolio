# =============================================================================
# 53_research_platoon_splits.R
# Research platoon splits for top Rule 5 hitter candidates
# =============================================================================

library(dplyr)

cat("📊 Rule 5 Hitter Platoon Split Research\n\n")

# Load top candidates
rankings <- read.csv("output/hitter_rankings_v2.csv", stringsAsFactors = FALSE)

# Top 10 candidates to research
top_candidates <- rankings %>%
  filter(rank <= 10) %>%
  select(rank, player_name, org, latest_position, latest_age, 
         recent_wrc, recent_iso, career_bb_pct, career_k_pct,
         role_projection, mlb_ready, flags)

cat("🎯 Top 10 Candidates for Platoon Split Research:\n\n")
print(top_candidates, row.names = FALSE)

cat("\n\n📋 RESEARCH TEMPLATE\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("For each player, research the following:\n\n")

cat("Sources:\n")
cat("1. Baseball Reference (MiLB splits): https://www.baseball-reference.com/register/player.fcgi?id=[playerid]\n")
cat("2. FanGraphs MiLB splits: https://www.fangraphs.com/players/[player-name]/milb\n")
cat("3. Perplexity search: '[Player Name] minor league platoon splits vs left vs right'\n\n")

cat("Key Questions:\n")
cat("- Bats: L or R?\n")
cat("- vs RHP: What's the wRC+/OPS/AVG?\n")
cat("- vs LHP: What's the wRC+/OPS/AVG?\n")
cat("- Platoon differential: Significant (>50 wRC+ gap)? Moderate (20-50)? Minimal (<20)?\n")
cat("- MLB Role Impact: Everyday player, platoon bat, bench depth?\n\n")

# Create research template CSV
research_template <- top_candidates %>%
  mutate(
    bats = "",
    vs_rhp_wrc = NA,
    vs_rhp_ops = NA,
    vs_lhp_wrc = NA,
    vs_lhp_ops = NA,
    platoon_differential = "",
    platoon_severity = "",  # SEVERE/MODERATE/MINIMAL
    mlb_role_impact = "",   # EVERYDAY/PLATOON/BENCH
    notes = "",
    research_source = ""
  ) %>%
  select(rank, player_name, org, bats, 
         vs_rhp_wrc, vs_rhp_ops, vs_lhp_wrc, vs_lhp_ops,
         platoon_differential, platoon_severity, mlb_role_impact,
         notes, research_source)

# Save template
write.csv(research_template, "config/platoon_splits_research_template.csv", row.names = FALSE)

cat("\n✅ Research template created: config/platoon_splits_research_template.csv\n")
cat("\n📌 Priority research order:\n")

priority <- top_candidates %>%
  mutate(
    priority_flag = case_when(
      grepl("OBP/Contact", role_projection) & recent_iso < 0.150 ~ "🔴 HIGH - Low power, needs platoon check",
      career_bb_pct > 0.12 ~ "🟡 MEDIUM - High BB%, check for patient vs both sides",
      recent_wrc > 130 ~ "🟢 LOW - Strong overall, splits less critical",
      TRUE ~ "⚪ STANDARD"
    )
  ) %>%
  select(rank, player_name, recent_wrc, recent_iso, role_projection, priority_flag)

print(priority, row.names = FALSE)

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("\n🔍 START WITH:\n")
cat("   1. Carlos Mendoza (Low power OBP guy - platoon risk HIGH)\n")
cat("   2. Victor Labrada (OBP/Contact - needs verification)\n")
cat("   3. Connor Charping (Catcher with .050 ISO - extreme concern)\n\n")

cat("💡 RED FLAGS to watch:\n")
cat("   - OBP-dependent hitters with <.130 ISO = likely platoon bat only\n")
cat("   - If wRC+ drops below 80 vs one side = severe platoon split\n")
cat("   - Catchers with no power often can't hit same-handed pitching\n\n")
