# 37_analyze_r5_benchmarks.R
# Analyze Rule 5 returned players to establish minimum benchmarks
# These are guys who WERE drafted - what made them draftable?

library(dplyr)
library(tidyr)

cat("\n📊 ANALYZING RULE 5 BENCHMARK PLAYERS\n")
cat("   What stats did R5 drafted players have?\n\n")

# Load the scraped data
r5_stats <- read.csv("data/r5_benchmark_hitting_2021_2025.csv", stringsAsFactors = FALSE)

cat("✅ Loaded", nrow(r5_stats), "season records for", length(unique(r5_stats$player_name)), "R5 players\n")
cat("   Columns:", ncol(r5_stats), "\n\n")

# Rename key columns by position to avoid duplicate name issues
# Based on CSV: Season, Team, Level, Age, G, PA, HR, R, RBI, SB, divider, BB%, K%, ISO, BABIP, divider, AVG, OBP, SLG, wOBA, xwOBA, wRC+
names(r5_stats)[1] <- "Season"
names(r5_stats)[2] <- "Team" 
names(r5_stats)[3] <- "Level"
names(r5_stats)[4] <- "Age"
names(r5_stats)[5] <- "G"
names(r5_stats)[6] <- "PA"
names(r5_stats)[7] <- "HR"
names(r5_stats)[8] <- "R"
names(r5_stats)[9] <- "RBI"
names(r5_stats)[10] <- "SB"
names(r5_stats)[12] <- "BB_pct"
names(r5_stats)[13] <- "K_pct"
names(r5_stats)[14] <- "ISO"
names(r5_stats)[15] <- "BABIP"
names(r5_stats)[17] <- "AVG"
names(r5_stats)[18] <- "OBP"
names(r5_stats)[19] <- "SLG"
names(r5_stats)[20] <- "wOBA"
names(r5_stats)[21] <- "xwOBA"
names(r5_stats)[22] <- "wRC_plus"
names(r5_stats)[28] <- "WAR"
names(r5_stats)[29] <- "playerId"
names(r5_stats)[30] <- "player_name"
names(r5_stats)[ncol(r5_stats)] <- "position_bucket"  # Last column

r5_clean <- r5_stats

cat("📋 Available metrics:\n")
cat("  ", paste(names(r5_clean)[1:20], collapse = ", "), "...\n\n")

# Focus on 2023-2024 (most recent before R5 draft)
r5_recent <- r5_clean %>%
  filter(Season %in% c(2023, 2024, 2025)) %>%
  filter(Level %in% c("AA", "AAA")) %>%  # MLB-proximate levels only
  mutate(
    # Clean percentage columns - remove "%" and convert to numeric
    BB_pct = as.numeric(gsub("%", "", BB_pct)),
    K_pct = as.numeric(gsub("%", "", K_pct)),
    # Convert other key stats
    PA = as.numeric(PA),
    wOBA = as.numeric(wOBA),
    wRC_plus = as.numeric(wRC_plus),
    ISO = as.numeric(ISO),
    AVG = as.numeric(AVG),
    OBP = as.numeric(OBP),
    SLG = as.numeric(SLG),
    SB = as.numeric(SB),
    Age = as.numeric(Age)
  )

cat("🎯 Filtered to 2023-2025 at AA/AAA:\n")
cat("   Records:", nrow(r5_recent), "\n")
cat("   Players:", length(unique(r5_recent$player_name)), "\n")
cat("   Sample wOBA values:", head(r5_recent$wOBA, 10), "\n")
cat("   Sample wRC+ values:", head(r5_recent$wRC_plus, 10), "\n\n")

# Calculate aggregate stats for each player
r5_benchmarks <- r5_recent %>%
  # Remove rows with NA in key metrics before grouping
  filter(!is.na(wOBA), !is.na(PA), PA > 0) %>%
  group_by(player_name, position_bucket) %>%
  summarize(
    seasons = n(),
    total_PA = sum(PA, na.rm = TRUE),
    avg_age = mean(Age, na.rm = TRUE),
    
    # Offensive metrics (weighted by PA)
    wOBA = weighted.mean(wOBA, PA, na.rm = TRUE),
    wRC_plus = weighted.mean(wRC_plus, PA, na.rm = TRUE),
    ISO = weighted.mean(ISO, PA, na.rm = TRUE),
    AVG = weighted.mean(AVG, PA, na.rm = TRUE),
    OBP = weighted.mean(OBP, PA, na.rm = TRUE),
    SLG = weighted.mean(SLG, PA, na.rm = TRUE),
    
    # Plate discipline
    BB_pct = weighted.mean(BB_pct, PA, na.rm = TRUE),
    K_pct = weighted.mean(K_pct, PA, na.rm = TRUE),
    
    # Speed/athleticism
    SB = sum(SB, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(desc(wRC_plus))

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  📊 RULE 5 BENCHMARK STATS (2023-2025 at AA/AAA)\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

print(r5_benchmarks, n = 50)

# Calculate minimum thresholds (25th percentile = what got you drafted)
thresholds <- r5_benchmarks %>%
  summarize(
    min_wOBA = quantile(wOBA, 0.25, na.rm = TRUE),
    min_wRC_plus = quantile(wRC_plus, 0.25, na.rm = TRUE),
    min_ISO = quantile(ISO, 0.25, na.rm = TRUE),
    min_OBP = quantile(OBP, 0.25, na.rm = TRUE),
    min_BB_pct = quantile(BB_pct, 0.25, na.rm = TRUE),
    max_K_pct = quantile(K_pct, 0.75, na.rm = TRUE),
    
    avg_wOBA = mean(wOBA, na.rm = TRUE),
    avg_wRC_plus = mean(wRC_plus, na.rm = TRUE),
    avg_ISO = mean(ISO, na.rm = TRUE),
    avg_OBP = mean(OBP, na.rm = TRUE)
  )

cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("  🎯 MINIMUM THRESHOLDS FOR R5 DRAFT (25th percentile)\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

cat("  wOBA:     ", round(thresholds$min_wOBA, 3), "(avg:", round(thresholds$avg_wOBA, 3), ")\n")
cat("  wRC+:     ", round(thresholds$min_wRC_plus, 0), "(avg:", round(thresholds$avg_wRC_plus, 0), ")\n")
cat("  ISO:      ", round(thresholds$min_ISO, 3), "(avg:", round(thresholds$avg_ISO, 3), ")\n")
cat("  OBP:      ", round(thresholds$min_OBP, 3), "(avg:", round(thresholds$avg_OBP, 3), ")\n")
cat("  BB%:      ", round(thresholds$min_BB_pct, 1), "% (min for draft consideration)\n")
cat("  K%:       ", round(thresholds$max_K_pct, 1), "% (max tolerable)\n\n")

# Save benchmarks
write.csv(r5_benchmarks, "output/r5_benchmark_analysis.csv", row.names = FALSE)
write.csv(thresholds, "output/r5_minimum_thresholds.csv", row.names = FALSE)

cat("✅ Analysis complete!\n")
cat("   📁 Benchmark analysis: output/r5_benchmark_analysis.csv\n")
cat("   📁 Minimum thresholds: output/r5_minimum_thresholds.csv\n\n")

cat("🎯 KEY INSIGHTS:\n")
cat("   • Players drafted in Rule 5 averaged wRC+ of", round(thresholds$avg_wRC_plus, 0), "\n")
cat("   • Minimum bar was wRC+ ~", round(thresholds$min_wRC_plus, 0), "\n")
cat("   • Use these thresholds to filter our 294 position players!\n\n")

cat("🏁 NEXT STEP: Apply these benchmarks to find draftable players!\n")
