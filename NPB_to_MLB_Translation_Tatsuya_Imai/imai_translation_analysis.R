# ==============================================================================
# NPB to MLB Translation Analysis: Tatsuya Imai
# ==============================================================================
# Author: NAB-hub
# Date: December 2025
# Purpose: Translate Tatsuya Imai's 2025 NPB performance to MLB equivalents
#          using FIP-based methodology and historical NPB import comps
# ==============================================================================

# Load required libraries
library(tidyverse)
library(knitr)
library(kableExtra)

# ==============================================================================
# 1. TATSUYA IMAI 2025 NPB STATISTICS
# ==============================================================================

imai_npb <- data.frame(
  Pitcher = "Tatsuya Imai",
  League = "NPB",
  Season = 2025,
  IP = 163.2,
  ERA = 1.92,
  FIP = 2.70,
  K = 178,
  BB = 45,
  HR = 6,
  H = 124,
  R = 38,
  ER = 35
)

# Calculate rate stats
imai_npb <- imai_npb %>%
  mutate(
    K_per_9 = (K / IP) * 9,
    BB_per_9 = (BB / IP) * 9,
    HR_per_9 = (HR / IP) * 9,
    K_BB_ratio = K / BB,
    WHIP = (H + BB) / IP
  )

print("Tatsuya Imai 2025 NPB Performance:")
print(imai_npb)

# ==============================================================================
# 2. NPB→MLB TRANSLATION FACTORS (Based on Historical Comps)
# ==============================================================================
# Comps: Yamamoto, Imanaga, Senga, Maeda (10,361 MLB pitches)

translation_factors <- data.frame(
  Metric = c("FIP", "K/9", "BB/9", "HR/9", "ERA+"),
  NPB_to_MLB_Adjustment = c(1.48, 0.6, 0.1, 0.5, NA),
  Method = c(
    "Multiply NPB FIP by 1.48",
    "Add 0.6 to NPB K/9",
    "Add 0.1 to NPB BB/9",
    "Add 0.5 to NPB HR/9",
    "Derived from translated FIP"
  )
)

print("NPB→MLB Translation Factors:")
print(translation_factors)

# ==============================================================================
# 3. APPLY TRANSLATION TO IMAI'S NPB STATS
# ==============================================================================

imai_mlb_translated <- data.frame(
  Pitcher = "Tatsuya Imai",
  League = "MLB (Projected)",
  Season = "Year 1",
  IP = 165,  # Conservative Year 1 workload
  FIP_NPB = imai_npb$FIP,
  FIP_MLB = imai_npb$FIP * 1.48,
  K_per_9_NPB = imai_npb$K_per_9,
  K_per_9_MLB = imai_npb$K_per_9 + 0.6,
  BB_per_9_NPB = imai_npb$BB_per_9,
  BB_per_9_MLB = imai_npb$BB_per_9 + 0.1,
  HR_per_9_NPB = imai_npb$HR_per_9,
  HR_per_9_MLB = imai_npb$HR_per_9 + 0.5
)

# Calculate projected counting stats for MLB Year 1
imai_mlb_translated <- imai_mlb_translated %>%
  mutate(
    K_MLB = round((K_per_9_MLB / 9) * IP),
    BB_MLB = round((BB_per_9_MLB / 9) * IP),
    HR_MLB = round((HR_per_9_MLB / 9) * IP)
  )

print("Imai NPB→MLB Translation:")
print(imai_mlb_translated)

# ==============================================================================
# 4. ERA+ PROJECTION (FIP-based)
# ==============================================================================
# Translated FIP of 3.99 ≈ 112 ERA+ (12% better than league average)

era_plus_projection <- data.frame(
  Year = c("Year 1", "Year 2", "Year 3-5 (Peak)"),
  ERA_Plus = c(112, 115, 122),
  Projected_IP = c(165, 175, 185),
  Projected_WAR = c(2.7, 3.0, 3.7),
  Notes = c(
    "Conservative ramp, 6→5 day transition",
    "Full workload acclimation",
    "Peak performance window (age 27-29)"
  )
)

print("ERA+ and WAR Projections:")
print(era_plus_projection)

# ==============================================================================
# 5. HISTORICAL NPB IMPORT COMPS
# ==============================================================================

npb_import_comps <- data.frame(
  Pitcher = c("Yoshinobu Yamamoto", "Shota Imanaga", "Kodai Senga", "Kenta Maeda", "Tatsuya Imai (Proj.)"),
  Age_at_MLB_Debut = c(25, 30, 30, 27, 26),
  Year_1_IP = c(174, 173, 166, 180, 165),
  Year_1_ERA_Plus = c(138, 126, 103, 114, 112),
  Year_1_FIP = c(2.92, 3.16, 3.76, 3.74, 3.99),
  Contract_AAV_M = c(36.5, 15, 15.3, 3, 21)
)

print("NPB Import Comparisons:")
print(npb_import_comps)

# ==============================================================================
# 6. ARSENAL BREAKDOWN (SIMPLIFIED PROJECTION)
# ==============================================================================

arsenal <- data.frame(
  Pitch = c("Fastball", "Reverse Slider", "Changeup", "Vulcan Change", "Splitter", "Curveball"),
  Grade = c(60, 65, 55, 55, 55, 40),
  Velocity_Range = c("92-95, T99", "84-87", "83-86", "82-85", "84-86", "75-78"),
  Usage_Pct = c(35, 30, 12, 8, 10, 5),
  Primary_Role = c(
    "Deceptive ride, sets up breaking balls",
    "Out pitch vs RHB (53% whiff rate)",
    "Keeps LHB honest",
    "Shows different look vs LHB",
    "Developing, trending up",
    "Needs development for 6-pitch mix"
  )
)

print("Imai Arsenal Summary:")
print(arsenal)

# ==============================================================================
# 7. WORKLOAD MANAGEMENT PLAN (YEAR 1)
# ==============================================================================

workload_plan <- data.frame(
  Period = c("April-May", "June-July", "August-September"),
  Rest_Days = c("6-day", "5-day (transition)", "5-day (full)"),
  Pitch_Count = c("80-90", "90-100", "95-105"),
  Target_Starts = c("8-10", "10-12", "8-10"),
  Target_IP = c("45-55", "55-65", "50-60")
)

print("Year 1 Workload Plan:")
print(workload_plan)

# ==============================================================================
# 8. VALUE PROJECTION
# ==============================================================================

value_summary <- data.frame(
  Category = c("Projected Contract", "Year 1-2 WAR", "Year 3-5 WAR (Peak)", "Value vs Market"),
  Estimate = c(
    "5 years, ~$105M ($21M AAV)",
    "2.5-3.0 WAR/year",
    "3.5-4.0 WAR/year",
    "Strong value vs older FAs (Burnes, Fried)"
  )
)

print("Value Summary:")
print(value_summary)

# ==============================================================================
# 9. FINAL PROJECTION SUMMARY
# ==============================================================================

final_projection <- data.frame(
  Metric = c("Role", "Year 1 IP", "Year 1 ERA+", "Year 1 FIP", "K/9", "BB/9", "HR/9", "WAR", "Grade"),
  Projection = c(
    "Impact #3 starter with #2 upside",
    "165",
    "112",
    "3.99",
    "10.4",
    "2.6",
    "1.2",
    "2.5-3.0",
    "A- (high-quality, strong value)"
  )
)

cat("\n=== TATSUYA IMAI MLB PROJECTION SUMMARY ===\n")
print(final_projection)

# ==============================================================================
# END OF ANALYSIS
# ==============================================================================
