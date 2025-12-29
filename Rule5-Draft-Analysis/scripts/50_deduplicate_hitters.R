# =============================================================================
# 50_deduplicate_hitters.R
# Clean FanGraphs hitter data and aggregate career stats
# =============================================================================

library(dplyr)

cat("🔧 Deduplicating and Aggregating Hitter Data...\n\n")

# Load raw data
raw <- read.csv("data/all_hitters_milb_2023_2025.csv", stringsAsFactors = FALSE)

cat(sprintf("📊 Raw data: %d rows\n", nrow(raw)))

# Check for exact duplicates
duplicates <- raw %>%
  group_by(Season, playerId, Level) %>%
  filter(n() > 1) %>%
  arrange(playerId, Season, Level)

if(nrow(duplicates) > 0) {
  cat(sprintf("⚠️  Found %d duplicate rows (same player/season/level)\n", nrow(duplicates)))
  
  # Remove exact duplicates, keep first instance
  raw <- raw %>%
    distinct(Season, playerId, Level, .keep_all = TRUE)
  
  cat(sprintf("✓ Deduplicated to %d rows\n", nrow(raw)))
}

# Clean and extract columns (FanGraphs format: "BB%BB% - Walk Percentage")
clean_data <- raw %>%
  select(
    Season, Team, Level, Age, 
    playerId, player_name, position_bucket, org,
    G = `GG...Games.Played`,
    PA = `PAPA...Plate.Appearances`,
    HR = `HRHR...Home.Runs`,
    R = `RR...Runs`,
    RBI = `RBIRBI...Runs.Batted.In`,
    SB = `SBSB...Stolen.Bases`,
    BB_pct_raw = `BB.BB....Walk.Percentage..BB.PA.`,
    K_pct_raw = `K.K....Strikeout.Percentage..SO.PA.`,
    ISO = `ISOISO...Isolated.Power..SLG.AVG.`,
    BABIP = `BABIPBABIP...Batting.Average.on.Balls.in.Play`,
    AVG = `AVGAVG...Batting.Average..H.AB.`,
    OBP = `OBPOBP...On.Base.Percentage`,
    SLG = `SLGSLG...Slugging.Percentage`,
    wOBA = `wOBAwOBA...Weighted.On.Base.Average..Linear.Weights.`,
    xwOBA = `xwOBAxwOBA...Expected.weighted.on.base.average`,
    wRC_plus = `wRC.wRC....Runs.per.PA.scaled.where.100.is.average..both.league.and.park.adjusted..based.on.wOBA`,
    BsR = `BsRBase.Running...Base.running.runs.above.average..includes.SB.or.CS`,
    Off = `OffOffense...Batting.and.Base.Running.combined..above.average.`,
    Def = `DefDefense...Fielding.and.Positional.Adjustment.combined..above.average.`,
    WAR = `WARWAR...Wins.Above.Replacement`
  ) %>%
  mutate(
    # Convert percentages to decimals
    BB_pct = as.numeric(gsub("%", "", BB_pct_raw)) / 100,
    K_pct = as.numeric(gsub("%", "", K_pct_raw)) / 100,
    
    # Convert numeric columns
    across(c(G, PA, HR, R, RBI, SB, ISO, BABIP, AVG, OBP, SLG, 
             wOBA, xwOBA, wRC_plus, BsR, Off, Def, WAR), as.numeric)
  ) %>%
  select(-BB_pct_raw, -K_pct_raw)  # Drop raw percentage strings

# Check unique players
unique_players <- clean_data %>%
  distinct(playerId, player_name, org) %>%
  nrow()

cat(sprintf("\n📈 Cleaned data summary:\n"))
cat(sprintf("   - Total seasons: %d\n", nrow(clean_data)))
cat(sprintf("   - Unique players: %d\n", unique_players))
cat(sprintf("   - Seasons per player (avg): %.1f\n", nrow(clean_data) / unique_players))

# Aggregate career stats
career_stats <- clean_data %>%
  group_by(playerId, player_name, org) %>%
  summarise(
    # Latest season info
    latest_season = max(Season, na.rm = TRUE),
    latest_level = Level[which.max(Season)],
    latest_age = Age[which.max(Season)],
    latest_position = position_bucket[which.max(Season)],
    
    # Career totals
    total_seasons = n_distinct(Season),
    total_pa = sum(PA, na.rm = TRUE),
    total_g = sum(G, na.rm = TRUE),
    total_hr = sum(HR, na.rm = TRUE),
    total_sb = sum(SB, na.rm = TRUE),
    
    # Career rates (weighted by PA)
    career_bb_pct = weighted.mean(BB_pct, PA, na.rm = TRUE),
    career_k_pct = weighted.mean(K_pct, PA, na.rm = TRUE),
    career_iso = weighted.mean(ISO, PA, na.rm = TRUE),
    career_babip = weighted.mean(BABIP, PA, na.rm = TRUE),
    career_avg = weighted.mean(AVG, PA, na.rm = TRUE),
    career_obp = weighted.mean(OBP, PA, na.rm = TRUE),
    career_slg = weighted.mean(SLG, PA, na.rm = TRUE),
    career_woba = weighted.mean(wOBA, PA, na.rm = TRUE),
    career_wrc_plus = weighted.mean(wRC_plus, PA, na.rm = TRUE),
    
    # Best season peaks
    best_woba = max(wOBA, na.rm = TRUE),
    best_wrc_plus = max(wRC_plus, na.rm = TRUE),
    best_iso = max(ISO, na.rm = TRUE),
    
    # Sample size flags
    has_aaa = any(Level == "AAA"),
    has_aa = any(Level == "AA"),
    pct_pa_aaa = sum(PA[Level == "AAA"], na.rm = TRUE) / total_pa * 100,
    pct_pa_aa = sum(PA[Level == "AA"], na.rm = TRUE) / total_pa * 100,
    
    .groups = "drop"
  ) %>%
  mutate(
    # Replace infinite values with NA
    across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))
  )

cat(sprintf("\n✅ Career stats aggregated: %d players\n", nrow(career_stats)))

# Save outputs
write.csv(clean_data, "data/hitters_cleaned_seasons.csv", row.names = FALSE)
write.csv(career_stats, "data/hitters_career_stats.csv", row.names = FALSE)

cat("\n📁 Files created:\n")
cat("   - data/hitters_cleaned_seasons.csv (all seasons, deduplicated)\n")
cat("   - data/hitters_career_stats.csv (career aggregates)\n\n")

# Summary stats
cat("📊 Career Stats Summary:\n")
cat(sprintf("   - AAA experience: %d players\n", sum(career_stats$has_aaa)))
cat(sprintf("   - AA+ experience: %d players\n", sum(career_stats$has_aa | career_stats$has_aaa)))
cat(sprintf("   - Avg PA: %.0f\n", mean(career_stats$total_pa)))
cat(sprintf("   - Avg wRC+: %.0f\n", mean(career_stats$career_wrc_plus, na.rm = TRUE)))
cat(sprintf("   - Players with 500+ PA: %d\n", sum(career_stats$total_pa >= 500)))

cat("\n✅ Deduplication complete!\n\n")
