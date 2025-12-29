# =============================================================================
# 13_tier2_scoring.R
# Tier 2: Intermediate Metrics (30% weight)
# Context & advanced stats: GB%, Age vs Level, HR/9, BABIP, LOB%, Consistency
# =============================================================================

library(dplyr)

# Load Tier 1 results
player_stats <- read.csv("data/player_stats_tier1.csv", stringsAsFactors = FALSE)

cat("Scoring Tier 2 metrics for", nrow(player_stats), "pitchers...\n")

# === Define Tier 2 Benchmarks ===

benchmarks_t2 <- list(
  # Ground ball percentage (higher is better - limits HR and extra base hits)
  gb_pct = list(elite = 50, good = 45, average = 40, poor = 35),
  
  # HR/9 (lower is better)
  hr9 = list(elite = 0.7, good = 1.0, average = 1.3, poor = 1.6),
  
  # BABIP (around .300 is average, extremes indicate luck)
  babip = list(optimal = 0.300, elite_range = 0.030, good_range = 0.050),
  
  # Age vs Level - Context-specific scoring for Rule 5 readiness
  # AAA: Experience matters (26-27 is ideal)
  # AA: Prospect age (22-24 is ideal)
  # A+: Youth matters (21-23 is ideal)
  age_vs_level = list(elite = -2.0, good = -1.0, average = 0, poor = 2.0, terrible = 4.0),  # Legacy - not used
  
  # ERA consistency (lower std dev is better)
  era_consistency = list(elite = 1.0, good = 1.5, average = 2.0, poor = 2.5)
)

# === Scoring Functions ===

score_metric_inverse <- function(value, bench) {
  # Check if terrible threshold exists
  has_terrible <- "terrible" %in% names(bench)
  
  if (has_terrible) {
    case_when(
      is.na(value) ~ 0,
      value <= bench$elite ~ 100,
      value <= bench$good ~ 80,
      value <= bench$average ~ 60,
      value <= bench$poor ~ 40,
      value <= bench$terrible ~ 10,
      TRUE ~ 5  # Basically disqualifying
    )
  } else {
    case_when(
      is.na(value) ~ 0,
      value <= bench$elite ~ 100,
      value <= bench$good ~ 80,
      value <= bench$average ~ 60,
      value <= bench$poor ~ 40,
      TRUE ~ 20
    )
  }
}

score_metric_direct <- function(value, bench) {
  case_when(
    is.na(value) ~ 0,
    value >= bench$elite ~ 100,
    value >= bench$good ~ 80,
    value >= bench$average ~ 60,
    value >= bench$poor ~ 40,
    TRUE ~ 20
  )
}

score_babip <- function(value, bench) {
  # BABIP around .300 is normal, extreme low/high suggests luck (good or bad)
  # We want players with sustainable BABIP (.280-.320)
  case_when(
    is.na(value) ~ 0,
    abs(value - bench$optimal) <= bench$elite_range ~ 100,  # .270-.330
    abs(value - bench$optimal) <= bench$good_range ~ 80,    # .250-.350
    TRUE ~ 60  # Extreme BABIP - regression likely
  )
}

# Level-specific age scoring for Rule 5 context
score_age_by_level <- function(age, level) {
  case_when(
    # AAA: Experience is valuable (Rule 5 ready veterans)
    level == "AAA" & age >= 24 & age <= 25 ~ 100,  # Elite: Young + experienced
    level == "AAA" & age >= 26 & age <= 27 ~ 80,   # Good: Prime Rule 5 targets
    level == "AAA" & age == 28 ~ 60,               # Average: Still MLB-ready
    level == "AAA" & age >= 29 ~ 40,               # Poor: Limited upside
    level == "AAA" & age <= 23 ~ 90,               # Very young for AAA (great)
    
    # AA: Prospect development curve
    level == "AA" & age >= 21 & age <= 22 ~ 100,   # Elite: Advanced for level
    level == "AA" & age >= 23 & age <= 24 ~ 80,    # Good: On track
    level == "AA" & age == 25 ~ 60,                # Average: Older but acceptable
    level == "AA" & age >= 26 ~ 40,                # Poor: Questionable prospect
    level == "AA" & age <= 20 ~ 95,                # Very young for AA
    
    # A+: Youth premium
    level == "A+" & age >= 21 & age <= 22 ~ 100,   # Elite: Still young
    level == "A+" & age == 23 ~ 80,                # Good: Last chance territory
    level == "A+" & age >= 24 & age <= 25 ~ 40,    # Poor: Org filler likely
    level == "A+" & age >= 26 ~ 20,                # Very poor: Disqualifying
    level == "A+" & age <= 20 ~ 100,               # Young for A+
    
    # Default fallback
    TRUE ~ 50
  )
}

# Calculate HR/9 from career stats
player_stats <- player_stats %>%
  mutate(
    career_hr9 = (total_games * 9) / pmax(total_ip, 1)  # Rough estimate if not in data
  )

# === Calculate Tier 2 Scores ===

player_stats <- player_stats %>%
  mutate(
    # Individual metric scores
    gb_pct_score = score_metric_direct(career_gb_pct, benchmarks_t2$gb_pct),
    hr9_score = score_metric_inverse(career_hr9, benchmarks_t2$hr9),
    babip_score = score_babip(career_gb_pct, benchmarks_t2$babip),  # Using GB% as proxy if BABIP missing
    age_level_score = score_age_by_level(latest_age, latest_level),  # New context-aware scoring
    consistency_score = score_metric_inverse(era_consistency, benchmarks_t2$era_consistency),
    
    # Workload/Experience bonus (more IP = more trust in stats)
    workload_score = case_when(
      total_ip >= 150 ~ 100,
      total_ip >= 100 ~ 85,
      total_ip >= 75 ~ 70,
      total_ip >= 50 ~ 55,
      TRUE ~ 40
    ),
    
    # Level of competition bonus (AAA > AA > A+)
    level_score = case_when(
      latest_level == "AAA" ~ 100,
      latest_level == "AA" ~ 80,
      latest_level == "A+" ~ 60,
      TRUE ~ 40
    ),
    
    # Tier 2 Composite Score
    # GB% and Age vs Level most important (25% each)
    # HR/9 and Consistency (15% each)
    # Workload and Level (10% each)
    tier2_score = (
      gb_pct_score * 0.25 +
      age_level_score * 0.25 +
      hr9_score * 0.15 +
      consistency_score * 0.15 +
      workload_score * 0.10 +
      level_score * 0.10
    ),
    
    # Grade assignment
    tier2_grade = case_when(
      tier2_score >= 85 ~ "A+",
      tier2_score >= 75 ~ "A",
      tier2_score >= 65 ~ "B+",
      tier2_score >= 55 ~ "B",
      tier2_score >= 45 ~ "C+",
      tier2_score >= 35 ~ "C",
      TRUE ~ "D"
    )
  )

# === Summary ===

cat("\n=== Tier 2 Scoring Summary ===\n")
cat("Mean Tier 2 Score:", round(mean(player_stats$tier2_score, na.rm = TRUE), 1), "\n")
cat("Median Tier 2 Score:", round(median(player_stats$tier2_score, na.rm = TRUE), 1), "\n")
cat("\nGrade Distribution:\n")
print(table(player_stats$tier2_grade))

cat("\nTop 10 Tier 2 Pitchers:\n")
top10_tier2 <- player_stats %>%
  arrange(desc(tier2_score)) %>%
  select(player_name, tier2_score, tier2_grade, latest_level, avg_age_vs_level, career_gb_pct) %>%
  head(10)
print(top10_tier2)

# === Save Results ===

write.csv(player_stats, "data/player_stats_tier2.csv", row.names = FALSE)

cat("\n✅ Tier 2 scoring complete!\n")
cat("  - Results saved to: data/player_stats_tier2.csv\n")
