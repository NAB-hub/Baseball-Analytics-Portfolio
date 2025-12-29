# Check why Pintar and Jordan didn't qualify

library(dplyr)

# Load the scraped data
hitters <- read.csv("data/all_hitters_milb_2023_2025.csv")

# Clean column names
names(hitters)[6] <- "PA"
names(hitters)[7] <- "HR"
names(hitters)[10] <- "SB"
names(hitters)[12] <- "BB_pct"
names(hitters)[13] <- "K_pct"
names(hitters)[14] <- "ISO"
names(hitters)[17] <- "AVG"
names(hitters)[18] <- "OBP"
names(hitters)[19] <- "SLG"
names(hitters)[20] <- "wOBA"
names(hitters)[22] <- "wRC_plus"

# Check Pintar
cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("  ANDREW PINTAR - Miami Marlins\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

pintar <- hitters %>%
  filter(grepl("Pintar", player_name, ignore.case = TRUE)) %>%
  filter(Level %in% c("AA", "AAA"), Season >= 2023) %>%
  arrange(Season, Level)

if (nrow(pintar) > 0) {
  print(pintar %>% select(Season, Level, Team, PA, wRC_plus, wOBA, ISO, OBP, HR, SB))
  
  # Aggregated stats
  pintar_agg <- pintar %>%
    filter(!is.na(wOBA), !is.na(PA), PA > 0) %>%
    summarise(
      total_PA = sum(PA, na.rm = TRUE),
      avg_wRC_plus = weighted.mean(wRC_plus, PA, na.rm = TRUE),
      avg_wOBA = weighted.mean(wOBA, PA, na.rm = TRUE),
      avg_ISO = weighted.mean(ISO, PA, na.rm = TRUE),
      avg_OBP = weighted.mean(OBP, PA, na.rm = TRUE),
      total_HR = sum(HR, na.rm = TRUE),
      total_SB = sum(SB, na.rm = TRUE)
    )
  
  cat("\nAGGREGATED 2023-2025 (AA/AAA):\n")
  cat(sprintf("  PA: %d | wRC+: %.0f | wOBA: %.3f | ISO: %.3f | OBP: %.3f\n",
              pintar_agg$total_PA, pintar_agg$avg_wRC_plus, pintar_agg$avg_wOBA,
              pintar_agg$avg_ISO, pintar_agg$avg_OBP))
  cat(sprintf("  HR: %d | SB: %d\n", pintar_agg$total_HR, pintar_agg$total_SB))
  
  # Check against thresholds
  cat("\nTHRESHOLD CHECK (wRC+ 85, wOBA .320, ISO .130, OBP .320, PA 250):\n")
  cat(sprintf("  wRC+ %s (need 85+)\n", ifelse(pintar_agg$avg_wRC_plus >= 85, "✅ PASS", "❌ FAIL")))
  cat(sprintf("  wOBA %s (need .320+)\n", ifelse(pintar_agg$avg_wOBA >= 0.320, "✅ PASS", "❌ FAIL")))
  cat(sprintf("  ISO %s (need .130+)\n", ifelse(pintar_agg$avg_ISO >= 0.130, "✅ PASS", "❌ FAIL")))
  cat(sprintf("  OBP %s (need .320+)\n", ifelse(pintar_agg$avg_OBP >= 0.320, "✅ PASS", "❌ FAIL")))
  cat(sprintf("  PA %s (need 250+)\n", ifelse(pintar_agg$total_PA >= 250, "✅ PASS", "❌ FAIL")))
} else {
  cat("❌ No AA/AAA data found 2023-2025\n")
}

# Check Jordan
cat("\n\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("  BLAZE JORDAN - St. Louis Cardinals (from Boston)\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

jordan <- hitters %>%
  filter(grepl("Blaze.*Jordan", player_name, ignore.case = TRUE)) %>%
  filter(Level %in% c("AA", "AAA"), Season >= 2023) %>%
  arrange(Season, Level)

if (nrow(jordan) > 0) {
  print(jordan %>% select(Season, Level, Team, PA, wRC_plus, wOBA, ISO, OBP, HR, SB))
  
  # Aggregated stats
  jordan_agg <- jordan %>%
    filter(!is.na(wOBA), !is.na(PA), PA > 0) %>%
    summarise(
      total_PA = sum(PA, na.rm = TRUE),
      avg_wRC_plus = weighted.mean(wRC_plus, PA, na.rm = TRUE),
      avg_wOBA = weighted.mean(wOBA, PA, na.rm = TRUE),
      avg_ISO = weighted.mean(ISO, PA, na.rm = TRUE),
      avg_OBP = weighted.mean(OBP, PA, na.rm = TRUE),
      total_HR = sum(HR, na.rm = TRUE),
      total_SB = sum(SB, na.rm = TRUE)
    )
  
  cat("\nAGGREGATED 2023-2025 (AA/AAA):\n")
  cat(sprintf("  PA: %d | wRC+: %.0f | wOBA: %.3f | ISO: %.3f | OBP: %.3f\n",
              jordan_agg$total_PA, jordan_agg$avg_wRC_plus, jordan_agg$avg_wOBA,
              jordan_agg$avg_ISO, jordan_agg$avg_OBP))
  cat(sprintf("  HR: %d | SB: %d\n", jordan_agg$total_HR, jordan_agg$total_SB))
  
  # Check against thresholds
  cat("\nTHRESHOLD CHECK (wRC+ 85, wOBA .320, ISO .130, OBP .320, PA 250):\n")
  cat(sprintf("  wRC+ %s (need 85+)\n", ifelse(jordan_agg$avg_wRC_plus >= 85, "✅ PASS", "❌ FAIL")))
  cat(sprintf("  wOBA %s (need .320+)\n", ifelse(jordan_agg$avg_wOBA >= 0.320, "✅ PASS", "❌ FAIL")))
  cat(sprintf("  ISO %s (need .130+)\n", ifelse(jordan_agg$avg_ISO >= 0.130, "✅ PASS", "❌ FAIL")))
  cat(sprintf("  OBP %s (need .320+)\n", ifelse(jordan_agg$avg_OBP >= 0.320, "✅ PASS", "❌ FAIL")))
  cat(sprintf("  PA %s (need 250+)\n", ifelse(jordan_agg$total_PA >= 250, "✅ PASS", "❌ FAIL")))
} else {
  cat("❌ No AA/AAA data found 2023-2025\n")
}

cat("\n\n💡 DEFENSIVE STATS:\n")
cat("FanGraphs MiLB stats do NOT include UZR or defensive metrics.\n")
cat("Defensive data would need to be sourced from:\n")
cat("  - Baseball America prospect lists (scouting grades)\n")
cat("  - MLB Pipeline (FV grades, arm/speed tools)\n")
cat("  - Manual scouting reports\n")
cat("  - Speed tools (SB totals are a proxy for speed)\n\n")
