# =============================================================================
# 16_pitcher_profiles.R
# Classify pitchers into actionable profiles based on their statistical tendencies
# =============================================================================

library(dplyr)

# Load top 25 targets
top25 <- read.csv("output/top25_targets.csv", stringsAsFactors = FALSE)

cat("Assigning pitcher profiles for", nrow(top25), "top prospects...\n")

# === Profile Classification Function ===

assign_profile <- function(k9, bb9, fip, gb_pct, role) {
  profiles <- c()
  
  # Handle NA values
  k9 <- ifelse(is.na(k9), 0, k9)
  bb9 <- ifelse(is.na(bb9), 999, bb9)
  fip <- ifelse(is.na(fip), 999, fip)
  gb_pct <- ifelse(is.na(gb_pct), 0, gb_pct)
  
  # Primary stuff indicator
  if (k9 >= 11.0) {
    profiles <- c(profiles, "Elite Strikeout Stuff")
  } else if (k9 >= 9.5) {
    profiles <- c(profiles, "Plus Strikeout Ability")
  }
  
  # Command profile
  if (bb9 <= 2.5) {
    profiles <- c(profiles, "Premium Command")
  } else if (bb9 <= 3.5) {
    profiles <- c(profiles, "Above-Average Control")
  } else if (bb9 > 4.5 && bb9 < 900) {
    profiles <- c(profiles, "Control Risk")
  }
  
  # Ground ball tendency
  if (gb_pct >= 50) {
    profiles <- c(profiles, "Ground Ball Specialist")
  } else if (gb_pct >= 45) {
    profiles <- c(profiles, "Above-Average Ground Balls")
  }
  
  # Results-based
  if (fip < 3.0) {
    profiles <- c(profiles, "Elite Results")
  } else if (fip < 3.5) {
    profiles <- c(profiles, "Strong Results")
  }
  
  # Role-specific profiles
  if (role == "Multi-Inning") {
    if (k9 >= 10 && fip < 3.5) {
      profiles <- c(profiles, "High-Leverage Multi-Inning Weapon")
    } else {
      profiles <- c(profiles, "Swing Man/Long Reliever")
    }
  } else {  # One-Inning
    if (k9 >= 11 && fip < 3.0) {
      profiles <- c(profiles, "Closer Upside")
    } else if (k9 >= 10) {
      profiles <- c(profiles, "High-Leverage Setup")
    } else {
      profiles <- c(profiles, "Middle Relief Specialist")
    }
  }
  
  # Create primary and secondary profiles
  if (length(profiles) == 0) {
    primary <- "Developmental Reliever"
    secondary <- "Needs refinement"
  } else {
    primary <- profiles[1]
    secondary <- if(length(profiles) > 1) paste(profiles[2:min(3, length(profiles))], collapse = ", ") else "Solid foundation"
  }
  
  # Create archetype description
  if (k9 >= 11 && bb9 <= 3.0) {
    archetype <- "Power arm with control"
  } else if (k9 >= 11) {
    archetype <- "Power arm, needs command refinement"
  } else if (bb9 <= 2.5 && k9 >= 9) {
    archetype <- "Command artist with quality stuff"
  } else if (gb_pct >= 50 && fip < 3.5) {
    archetype <- "Ground ball machine, limits damage"
  } else if (fip < 3.0) {
    archetype <- "Results-oriented, gets outs consistently"
  } else {
    archetype <- "Developing profile, shows promise"
  }
  
  return(list(
    primary_profile = primary,
    secondary_traits = secondary,
    archetype = archetype
  ))
}

# === Generate Profiles for Top 25 ===

profiles_list <- list()

for (i in 1:nrow(top25)) {
  prof <- assign_profile(
    k9 = ifelse(is.na(top25$career_k9[i]), 0, top25$career_k9[i]),
    bb9 = ifelse(is.na(top25$career_bb9[i]), 999, top25$career_bb9[i]),
    fip = ifelse(is.na(top25$career_fip[i]), 999, top25$career_fip[i]),
    gb_pct = ifelse(is.na(top25$career_gb_pct[i]) || is.null(top25$career_gb_pct[i]), 0, top25$career_gb_pct[i]),
    role = ifelse(is.na(top25$primary_role[i]) || top25$primary_role[i] == "", "One-Inning", top25$primary_role[i])
  )
  
  profiles_list[[i]] <- data.frame(
    player_name = top25$player_name[i],
    primary_profile = prof$primary_profile,
    secondary_traits = prof$secondary_traits,
    archetype = prof$archetype,
    stringsAsFactors = FALSE
  )
}

profiles_df <- bind_rows(profiles_list)

# Merge with top 25
top25_with_profiles <- top25 %>%
  left_join(profiles_df, by = "player_name")

# === Display Results ===

cat("\n=== Pitcher Profiles for Top 25 Prospects ===\n\n")
for (i in 1:nrow(top25_with_profiles)) {
  cat(sprintf("%d. %s (%s)\n", i, 
              top25_with_profiles$player_name[i],
              top25_with_profiles$primary_role[i]))
  cat(sprintf("   Profile: %s\n", top25_with_profiles$primary_profile[i]))
  cat(sprintf("   Traits: %s\n", top25_with_profiles$secondary_traits[i]))
  cat(sprintf("   Archetype: %s\n\n", top25_with_profiles$archetype[i]))
}

# === Save Results ===

write.csv(top25_with_profiles, "output/top25_with_profiles.csv", row.names = FALSE)

cat("\n✅ Pitcher profiles generated!\n")
cat("  - Results saved to: output/top25_with_profiles.csv\n")

cat("\n✅ Pitcher profiles generated!\n")
cat("  - Results saved to: output/top20_with_profiles.csv\n")
