# 39_validate_targets.R
# Validate our known targets (Pintar, Jordan) are in the data
# Then explore distribution to set better thresholds

library(dplyr)
library(tidyr)

cat("\n🔍 VALIDATING TARGET PLAYERS & EXPLORING DATA\n\n")

# Load scraped data
all_stats <- read.csv("data/all_hitters_milb_2023_2025.csv", stringsAsFactors = FALSE)

cat("✅ Loaded", nrow(all_stats), "season records\n\n")

# Clean column names
names(all_stats)[1] <- "Season"
names(all_stats)[2] <- "Team" 
names(all_stats)[3] <- "Level"
names(all_stats)[4] <- "Age"
names(all_stats)[5] <- "G"
names(all_stats)[6] <- "PA"
names(all_stats)[7] <- "HR"
names(all_stats)[8] <- "R"
names(all_stats)[9] <- "RBI"
names(all_stats)[10] <- "SB"
names(all_stats)[12] <- "BB_pct"
names(all_stats)[13] <- "K_pct"
names(all_stats)[14] <- "ISO"
names(all_stats)[15] <- "BABIP"
names(all_stats)[17] <- "AVG"
names(all_stats)[18] <- "OBP"
names(all_stats)[19] <- "SLG"
names(all_stats)[20] <- "wOBA"
names(all_stats)[21] <- "xwOBA"
names(all_stats)[22] <- "wRC_plus"
names(all_stats)[28] <- "WAR"
names(all_stats)[29] <- "playerId"
names(all_stats)[30] <- "player_name"

# Clean numeric columns
all_stats <- all_stats %>%
  mutate(
    BB_pct = as.numeric(gsub("%", "", BB_pct)),
    K_pct = as.numeric(gsub("%", "", K_pct)),
    PA = as.numeric(PA),
    wOBA = as.numeric(wOBA),
    wRC_plus = as.numeric(wRC_plus),
    ISO = as.numeric(ISO),
    AVG = as.numeric(AVG),
    OBP = as.numeric(OBP),
    SLG = as.numeric(SLG),
    SB = as.numeric(SB),
    HR = as.numeric(HR),
    Age = as.numeric(Age)
  )

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  🎯 VALIDATING KEY TARGETS\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Check Andrew Pintar
pintar <- all_stats %>% filter(grepl("Pintar", player_name, ignore.case = TRUE))
if (nrow(pintar) > 0) {
  cat("✅ ANDREW PINTAR FOUND:\n")
  print(pintar %>% select(player_name, Season, Level, Team, PA, AVG, OBP, SLG, wOBA, wRC_plus, HR, SB))
  cat("\n")
} else {
  cat("❌ Andrew Pintar NOT FOUND\n\n")
}

# Check Blaze Jordan
jordan <- all_stats %>% filter(grepl("Blaze.*Jordan|Jordan.*Blaze", player_name, ignore.case = TRUE))
if (nrow(jordan) > 0) {
  cat("✅ BLAZE JORDAN FOUND:\n")
  print(jordan %>% select(player_name, Season, Level, Team, PA, AVG, OBP, SLG, wOBA, wRC_plus, HR, SB))
  cat("\n")
} else {
  cat("❌ Blaze Jordan NOT FOUND\n\n")
}

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  📊 DATA QUALITY CHECK (AA/AAA 2023-2025)\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Filter to AA/AAA recent seasons
recent_aa_aaa <- all_stats %>%
  filter(Season %in% c(2023, 2024, 2025)) %>%
  filter(Level %in% c("AA", "AAA")) %>%
  filter(!is.na(wOBA), !is.na(PA), PA > 0)

cat("📍 AA/AAA 2023-2025 with valid stats:\n")
cat("   Total records:", nrow(recent_aa_aaa), "\n")
cat("   Unique players:", length(unique(recent_aa_aaa$player_name)), "\n\n")

# Aggregate by player for distribution analysis
player_aggregates <- recent_aa_aaa %>%
  group_by(player_name, position_bucket, org) %>%
  summarize(
    total_PA = sum(PA, na.rm = TRUE),
    seasons = n(),
    avg_age = mean(Age, na.rm = TRUE),
    
    wOBA = weighted.mean(wOBA, PA, na.rm = TRUE),
    wRC_plus = weighted.mean(wRC_plus, PA, na.rm = TRUE),
    ISO = weighted.mean(ISO, PA, na.rm = TRUE),
    OBP = weighted.mean(OBP, PA, na.rm = TRUE),
    BB_pct = weighted.mean(BB_pct, PA, na.rm = TRUE),
    K_pct = weighted.mean(K_pct, PA, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(total_PA >= 100)  # Minimum sample

cat("📊 Players with 100+ PA at AA/AAA (2023-2025):", nrow(player_aggregates), "\n\n")

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  📈 STAT DISTRIBUTIONS (for threshold setting)\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Calculate percentiles
cat("wRC+ Distribution:\n")
cat("  10th percentile:", round(quantile(player_aggregates$wRC_plus, 0.10, na.rm = TRUE), 0), "\n")
cat("  25th percentile:", round(quantile(player_aggregates$wRC_plus, 0.25, na.rm = TRUE), 0), "\n")
cat("  50th percentile:", round(quantile(player_aggregates$wRC_plus, 0.50, na.rm = TRUE), 0), "\n")
cat("  75th percentile:", round(quantile(player_aggregates$wRC_plus, 0.75, na.rm = TRUE), 0), "\n")
cat("  90th percentile:", round(quantile(player_aggregates$wRC_plus, 0.90, na.rm = TRUE), 0), "\n\n")

cat("wOBA Distribution:\n")
cat("  25th percentile:", round(quantile(player_aggregates$wOBA, 0.25, na.rm = TRUE), 3), "\n")
cat("  50th percentile:", round(quantile(player_aggregates$wOBA, 0.50, na.rm = TRUE), 3), "\n")
cat("  75th percentile:", round(quantile(player_aggregates$wOBA, 0.75, na.rm = TRUE), 3), "\n\n")

cat("ISO Distribution:\n")
cat("  25th percentile:", round(quantile(player_aggregates$ISO, 0.25, na.rm = TRUE), 3), "\n")
cat("  50th percentile:", round(quantile(player_aggregates$ISO, 0.50, na.rm = TRUE), 3), "\n")
cat("  75th percentile:", round(quantile(player_aggregates$ISO, 0.75, na.rm = TRUE), 3), "\n\n")

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  🎯 R5 BENCHMARK COMPARISON\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Load R5 thresholds
thresholds <- read.csv("output/r5_minimum_thresholds.csv", stringsAsFactors = FALSE)

cat("R5 Returned Players (minimum bar to GET DRAFTED):\n")
cat("  wRC+:", round(thresholds$min_wRC_plus, 0), "\n")
cat("  wOBA:", round(thresholds$min_wOBA, 3), "\n")
cat("  ISO:", round(thresholds$min_ISO, 3), "\n\n")

# How many meet R5 benchmarks?
r5_qualified <- player_aggregates %>%
  filter(
    total_PA >= 200,
    wRC_plus >= thresholds$min_wRC_plus,
    wOBA >= thresholds$min_wOBA,
    ISO >= thresholds$min_ISO,
    OBP >= thresholds$min_OBP,
    BB_pct >= thresholds$min_BB_pct,
    K_pct <= thresholds$max_K_pct
  )

cat("Players meeting R5 thresholds (200+ PA):", nrow(r5_qualified), "\n")
cat("  By position:\n")
print(r5_qualified %>% count(position_bucket) %>% arrange(desc(n)))

cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("  💡 RECOMMENDATION\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

cat("R5 RETURNED players are guys who got drafted but SENT BACK.\n")
cat("We want players good enough to be KEPT on 40-man rosters!\n\n")

cat("SUGGESTED THRESHOLDS (more aggressive):\n")
cat("  wRC+:    85+ (league average to above)\n")
cat("  wOBA:    .320+ (solid offensive contributor)\n")
cat("  ISO:     .130+ (some power)\n")
cat("  OBP:     .320+ (get on base)\n")
cat("  Min PA:  250+ (decent sample)\n\n")

# Test suggested thresholds
suggested_qualified <- player_aggregates %>%
  filter(
    total_PA >= 250,
    wRC_plus >= 85,
    wOBA >= .320,
    ISO >= .130,
    OBP >= .320
  )

cat("Players meeting SUGGESTED thresholds:", nrow(suggested_qualified), "\n")
cat("  By position:\n")
print(suggested_qualified %>% count(position_bucket) %>% arrange(desc(n)))

cat("\n🎯 What do you want to do?\n")
cat("   A) Use R5 benchmarks (stricter, fewer players)\n")
cat("   B) Use suggested thresholds (more aggressive, better targets)\n")
cat("   C) Custom thresholds (you decide)\n")
