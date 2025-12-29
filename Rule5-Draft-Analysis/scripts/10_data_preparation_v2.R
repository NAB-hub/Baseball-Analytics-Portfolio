# =============================================================================
# 10_data_preparation_v2.R
# Clean and prepare MiLB stats for analysis (SIMPLIFIED VERSION)
# =============================================================================

library(dplyr)
library(tidyr)

# Load the cleaned MiLB stats (already processed from mastersheet)
milb_clean <- read.csv("data/cleaned_milb_stats.csv", stringsAsFactors = FALSE)

# === Add AAA Park Factor Context ===
# PCL (Pacific Coast League) has high-altitude parks that inflate offensive stats
# IL (International League) plays at normal elevation - more reliable pitcher stats

# Map MLB teams to AAA affiliates and leagues
aaa_league_map <- data.frame(
  team = c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET",
           "HOU", "KC", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK",
           "PHI", "PIT", "SD", "SF", "SEA", "STL", "TB", "TEX", "TOR", "WSH"),
  aaa_affiliate = c("Reno", "Gwinnett", "Norfolk", "Worcester", "Iowa", "Charlotte", 
                    "Louisville", "Columbus", "Albuquerque", "Toledo",
                    "Sugar Land", "Omaha", "Salt Lake", "Oklahoma City", "Jacksonville", 
                    "Nashville", "St. Paul", "Syracuse", "Scranton", "Las Vegas",
                    "Lehigh Valley", "Indianapolis", "El Paso", "Sacramento", "Tacoma", 
                    "Memphis", "Durham", "Round Rock", "Buffalo", "Rochester"),
  aaa_league = c("PCL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", "PCL", "IL",
                 "PCL", "IL", "PCL", "PCL", "IL", "IL", "IL", "IL", "IL", "PCL",
                 "IL", "IL", "PCL", "PCL", "PCL", "IL", "IL", "PCL", "IL", "IL"),
  park_context = c("Hitter-friendly (altitude)", "Pitcher-friendly", "Pitcher-friendly", 
                   "Pitcher-friendly", "Pitcher-friendly", "Pitcher-friendly", "Pitcher-friendly", 
                   "Pitcher-friendly", "Hitter-friendly (altitude)", "Pitcher-friendly",
                   "Hitter-friendly (altitude)", "Pitcher-friendly", "Hitter-friendly (altitude)", 
                   "Hitter-friendly (altitude)", "Pitcher-friendly", "Pitcher-friendly", 
                   "Pitcher-friendly", "Pitcher-friendly", "Pitcher-friendly", "Hitter-friendly (altitude)",
                   "Pitcher-friendly", "Pitcher-friendly", "Hitter-friendly (altitude)", 
                   "Hitter-friendly (altitude)", "Hitter-friendly (altitude)", "Pitcher-friendly", 
                   "Pitcher-friendly", "Hitter-friendly (altitude)", "Pitcher-friendly", "Pitcher-friendly"),
  stringsAsFactors = FALSE
)

# Add league info to each season record
# Use mapping via match() to avoid relying on a prior join creating columns
milb_clean <- milb_clean %>%
  mutate(
    # Only apply park context to AAA using the mapping table
    aaa_league = ifelse(
      level == "AAA",
      aaa_league_map$aaa_league[match(team, aaa_league_map$team)],
      NA_character_
    ),
    park_context = ifelse(
      level == "AAA",
      aaa_league_map$park_context[match(team, aaa_league_map$team)],
      "N/A (not AAA)"
    ),
    pcl_pitcher = ifelse(level == "AAA" & aaa_league == "PCL", TRUE, FALSE)
  )

# === Detect batted-ball columns (HR/FB, FB%) if present and standardize names ===
hr_fb_col <- names(milb_clean)[grepl("hr[_ ]?fb|home[_ ]run[_ ]to[_ ]fly|HR/FB", names(milb_clean), ignore.case = TRUE)]
fb_pct_col <- names(milb_clean)[grepl("(^fb$|fb[_%]|fly[_ ]ball|FB%|FB_pct|fbpercent)", names(milb_clean), ignore.case = TRUE)]

if(length(hr_fb_col) > 0) {
  hr_fb_col <- hr_fb_col[1]
  milb_clean <- milb_clean %>% mutate(hr_fb_source = .data[[hr_fb_col]])
  cat("Detected HR/FB column:", hr_fb_col, "-> using hr_fb_source for aggregation\n")
} else {
  milb_clean <- milb_clean %>% mutate(hr_fb_source = NA_real_)
  cat("No HR/FB column detected; hr_fb_source set to NA\n")
}

if(length(fb_pct_col) > 0) {
  fb_pct_col <- fb_pct_col[1]
  milb_clean <- milb_clean %>% mutate(fb_pct_source = .data[[fb_pct_col]])
  cat("Detected FB% column:", fb_pct_col, "-> using fb_pct_source for aggregation\n")
} else {
  # FB% not present explicitly; leave as NA (we could compute from LD%+GB% if LD available)
  milb_clean <- milb_clean %>% mutate(fb_pct_source = NA_real_)
  cat("No FB% column detected; fb_pct_source set to NA\n")
}

cat("Loaded", nrow(milb_clean), "rows of MiLB stats\n")
cat("Columns found:", ncol(milb_clean), "\n")
cat("Players:", length(unique(milb_clean$player_name)), "\n")
cat("Seasons:", paste(sort(unique(milb_clean$season)), collapse = ", "), "\n")

# Check handedness
if ("pitcher_hand" %in% names(milb_clean)) {
  cat("Pitcher handedness:", 
      sum(milb_clean$pitcher_hand == "L", na.rm = TRUE), "LHP,",
      sum(milb_clean$pitcher_hand == "R", na.rm = TRUE), "RHP\n\n")
} else {
  cat("WARNING: pitcher_hand column not found!\n\n")
}

# === Aggregate to player level ===
player_stats <- milb_clean %>%
  group_by(playerid, player_name) %>%
  summarise(
    # Pitcher handedness (take first non-NA value)
    pitcher_hand = first(na.omit(pitcher_hand)),
    
    # Park context flags
    latest_aaa_league = last(aaa_league[level == "AAA" & !is.na(aaa_league)]),
    latest_park_context = last(park_context[level == "AAA" & !is.na(park_context)]),
    seasons_in_pcl = sum(pcl_pitcher, na.rm = TRUE),
    pct_ip_in_pcl = sum(ip[pcl_pitcher == TRUE], na.rm = TRUE) / sum(ip[level == "AAA"], na.rm = TRUE) * 100,
    
    # Totals
    total_ip = sum(ip, na.rm = TRUE),
    total_games = sum(g, na.rm = TRUE),
    total_starts = sum(gs, na.rm = TRUE),
    total_saves = sum(sv, na.rm = TRUE),
    
    # Career rates (LEVEL-WEIGHTED: AAA=3x, AA=2x, A+=0.3x)
    career_era = weighted.mean(era, ip * level_weight, na.rm = TRUE),
    career_fip = weighted.mean(fip, ip * level_weight, na.rm = TRUE),
    career_k9 = weighted.mean(k_9, ip * level_weight, na.rm = TRUE),
    career_bb9 = weighted.mean(bb_9, ip * level_weight, na.rm = TRUE),
    career_hr_9 = weighted.mean(hr_9, ip * level_weight, na.rm = TRUE),
    career_gb_pct = weighted.mean(gb_pct, ip * level_weight, na.rm = TRUE),
    career_hr_fb = weighted.mean(hr_fb_source, ip * level_weight, na.rm = TRUE),
    career_fb_pct = weighted.mean(fb_pct_source, ip * level_weight, na.rm = TRUE),
    career_v_fa = weighted.mean(v_f_av_fa_fourseam_fastball_velocity_pitch_info, 
                                ip * level_weight, na.rm = TRUE),
    
    # Best performances
    best_era = min(era, na.rm = TRUE),
    best_fip = min(fip, na.rm = TRUE),
    best_k9 = max(k_9, na.rm = TRUE),
    
    # Latest info
    latest_season = max(season, na.rm = TRUE),
    latest_level = level[which.max(season)],
    latest_age = age[which.max(season)],
    latest_ip = ip[which.max(season)],
    
    # Track record
    total_seasons = n(),
    
    # Context metrics
    avg_age_vs_level = mean(age_vs_level, na.rm = TRUE),
    era_consistency = sd(era, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    avg_ip_per_game = total_ip / total_games,
    career_ip_per_game = total_ip / total_games,
    starter_percentage = (total_starts / total_games) * 100,
    career_whip = NA_real_,  # Placeholder if not in data
    
    # CLASSIFY AS STARTER OR RELIEVER
    role = case_when(
      starter_percentage >= 50 ~ "Starter",
      starter_percentage < 20 ~ "Reliever",
      TRUE ~ "Swing"  # Can do both
    )
  )

cat("\nAggregated to", nrow(player_stats), "players\n")

# === Save results ===
write.csv(milb_clean, "data/cleaned_milb_stats.csv", row.names = FALSE)
write.csv(player_stats, "data/player_career_stats.csv", row.names = FALSE)

cat("\n✅ Data preparation complete!\n")
cat("  - Cleaned stats: data/cleaned_milb_stats.csv\n")
cat("  - Player aggregates: data/player_career_stats.csv\n")
