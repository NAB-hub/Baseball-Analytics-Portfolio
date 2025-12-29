# =============================================================================
# 99_master_pipeline.R
# Run complete analysis pipeline from data cleaning through scouting reports
# Execute this script after MiLB data scraping is complete
# =============================================================================

cat("================================================================================\n")
cat("RULE 5 RELIEF PITCHER ANALYTICS - MASTER PIPELINE\n")
cat("================================================================================\n\n")

start_time <- Sys.time()

# === Step 1: Data Preparation ===
cat("\n[1/7] Running data preparation and cleaning...\n")
tryCatch({
  source("scripts/10_data_preparation_v2.R")
  cat("✅ Step 1 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 1:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 2: Pitcher Classification ===
cat("\n[2/7] Classifying pitchers (Multi-Inning vs One-Inning)...\n")
tryCatch({
  source("scripts/11_pitcher_classification.R")
  cat("✅ Step 2 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 2:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 3: Tier 1 Scoring ===
cat("\n[3/7] Calculating Tier 1 scores (Foundational Metrics)...\n")
tryCatch({
  source("scripts/12_tier1_scoring.R")
  cat("✅ Step 3 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 3:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 4: Tier 2 Scoring ===
cat("\n[4/7] Calculating Tier 2 scores (Intermediate Metrics)...\n")
tryCatch({
  source("scripts/13_tier2_scoring.R")
  cat("✅ Step 4 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 4:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 5: Tier 3 Scoring ===
cat("\n[5/7] Calculating Tier 3 scores (Projection Factors)...\n")
tryCatch({
  source("scripts/14_tier3_scoring.R")
  cat("✅ Step 5 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 5:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 6: Composite Rankings ===
cat("\n[6/7] Generating composite rankings and Top 20...\n")
tryCatch({
  source("scripts/15_composite_rankings.R")
  cat("✅ Step 6 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 6:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 7: Pitcher Profiles ===
cat("\n[7/7] Assigning pitcher profiles...\n")
tryCatch({
  source("scripts/16_comparable_players.R")
  cat("✅ Step 7 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 7:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 8: Scouting Reports ===
cat("\n[8/9] Generating scouting reports for Top 25...\n")
tryCatch({
  source("scripts/17_scouting_reports.R")
  cat("✅ Step 8 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 8:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 9: MLB Roster Projection ===
cat("\n[9/13] Projecting MLB roster worthiness (can they stick?)...\n")
tryCatch({
  source("scripts/19_mlb_roster_projection.R")
  cat("✅ Step 9 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 9:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 10: LHP Specialist Rankings ===
cat("\n[10/13] Creating LHP specialist rankings...\n")
tryCatch({
  source("scripts/18_lhp_specialist_rankings.R")
  cat("✅ Step 10 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 10:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 11: Create Visualizations ===
cat("\n[11/13] Creating visualizations for PDF report...\n")
tryCatch({
  source("scripts/20_create_visualizations.R")
  cat("✅ Step 11 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 11:", e$message, "\n")
  stop("Pipeline halted")
})

# === Step 12: Injury Risk Analysis ===
cat("\n[12/13] Analyzing injury risk indicators...\n")
tryCatch({
  source("scripts/12_injury_risk_analysis.R")
  cat("✅ Step 12 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 12:", e$message, "\n")
  cat("Note: Injury risk analysis failed but pipeline will continue\n")
})

# === Step 13: Generate PDF Report ===
cat("\n[13/13] Generating HTML/PDF report...\n")
tryCatch({
  source("scripts/21_generate_html_report.R")
  cat("✅ Step 13 complete\n")
}, error = function(e) {
  cat("❌ Error in Step 13:", e$message, "\n")
  cat("Note: Report generation requires rmarkdown package.\n")
})

# === Pipeline Complete ===
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

cat("\n================================================================================\n")
cat("✅ PIPELINE COMPLETE!\n")
cat("================================================================================\n")
cat(sprintf("Total execution time: %.1f minutes\n\n", elapsed))

cat("OUTPUT FILES GENERATED:\n")
cat("  Data Files:\n")
cat("    • data/cleaned_milb_stats.csv\n")
cat("    • data/player_career_stats.csv\n")
cat("    • data/player_stats_classified.csv\n")
cat("    • data/player_stats_tier1.csv\n")
cat("    • data/player_stats_tier2.csv\n")
cat("    • data/player_stats_tier3.csv\n\n")

cat("  Rankings:\n")
cat("    • output/overall_rankings.csv\n")
cat("    • output/multi_inning_rankings.csv\n")
cat("    • output/one_inning_rankings.csv\n")
cat("    • output/top25_targets.csv\n")
cat("    • output/top20_targets.csv\n")
cat("    • output/top25_with_profiles.csv\n")
cat("    • output/top10_lhp_specialists.csv\n")
cat("    • output/all_lhp_rankings.csv\n\n")

cat("  Scouting Reports:\n")
cat("    • reports/TOP25_MASTER_SCOUTING_REPORT.txt\n")
cat("    • reports/top25_summary_table.csv\n")
cat("    • reports/scouting_report_*.txt (25 individual reports)\n\n")

cat("  Visualizations:\n")
cat("    • output/plots/01_top25_rankings.png\n")
cat("    • output/plots/02_tier_scatter.png\n")
cat("    • output/plots/03_pitcher_type_distribution.png\n")
cat("    • output/plots/04_lhp_specialists.png\n")
cat("    • output/plots/05_grade_distribution.png\n")
cat("    • output/plots/06_top15_heatmap.png\n")
cat("    • output/plots/07_top10_metrics.png\n\n")

cat("  📱 MOBILE-READY PDF REPORT:\n")
cat("    • reports/Rule5_Draft_Report.pdf\n")
cat("    • Complete analysis with visualizations, rankings, and methodology\n")
cat("    • Optimized for phone viewing during front office meetings\n\n")

cat("NEXT STEPS:\n")
cat("  1. Transfer reports/Rule5_Draft_Report.pdf to your phone 📱\n")
cat("  2. Review output/top25_targets.csv for quick reference\n")
cat("  3. Check output/top10_lhp_specialists.csv for LHP matchup options\n")
cat("  4. Read reports/TOP25_MASTER_SCOUTING_REPORT.txt for text-based details\n")
cat("  5. You're ready for front office conversations! 🎯\n\n")

cat("================================================================================\n")
