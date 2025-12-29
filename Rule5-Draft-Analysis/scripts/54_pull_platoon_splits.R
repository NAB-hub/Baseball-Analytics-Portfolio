# =============================================================================
# 54_pull_platoon_splits.R
# Pull MiLB game logs from FanGraphs and calculate platoon splits
# Using baseballr package
# =============================================================================

# Install baseballr if needed
if (!require("baseballr")) {
  install.packages("baseballr")
}

library(baseballr)
library(dplyr)
library(tidyr)

cat("📊 Pulling MiLB Platoon Splits from FanGraphs\n\n")

# Load our top candidates
rankings <- read.csv("output/hitter_rankings_v2.csv", stringsAsFactors = FALSE)

top10 <- rankings %>%
  filter(rank <= 10) %>%
  select(rank, player_name, playerId, org, latest_position, latest_age, recent_wrc)

cat("🎯 Top 10 Players to Research:\n\n")
print(top10, row.names = FALSE)

cat("\n⚙️  Attempting to pull game logs...\n\n")

# Function to get platoon splits for a player
get_platoon_splits <- function(player_name, player_id, seasons = c(2024, 2025)) {
  
  cat(sprintf("   Pulling data for %s (ID: %s)...\n", player_name, player_id))
  
  tryCatch({
    # Pull game logs for 2024-2025
    all_games <- data.frame()
    
    for (season in seasons) {
      games <- fg_milb_batter_game_logs(playerid = player_id, year = season)
      
      if (nrow(games) > 0) {
        games$season <- season
        all_games <- bind_rows(all_games, games)
      }
    }
    
    if (nrow(all_games) == 0) {
      cat(sprintf("      ⚠️  No game logs found for %s\n", player_name))
      return(NULL)
    }
    
    cat(sprintf("      ✓ Found %d games\n", nrow(all_games)))
    
    # Calculate splits if we have pitcher handedness data
    # Note: baseballr may or may not include pitcher handedness in game logs
    # If not, we'll need to join separately
    
    return(all_games)
    
  }, error = function(e) {
    cat(sprintf("      ❌ Error pulling data for %s: %s\n", player_name, e$message))
    return(NULL)
  })
}

# Pull data for each player
platoon_data <- list()

for (i in 1:nrow(top10)) {
  player <- top10[i, ]
  
  # FanGraphs uses numeric IDs, need to extract from playerId (format: sa3014468)
  # Try extracting numeric portion
  fg_id <- gsub("sa", "", player$playerId)
  
  games <- get_platoon_splits(player$player_name, fg_id)
  
  if (!is.null(games)) {
    platoon_data[[player$player_name]] <- games
  }
  
  # Be respectful to API
  Sys.sleep(2)
}

cat("\n📊 Summary:\n")
cat(sprintf("   - Players with data: %d / %d\n", length(platoon_data), nrow(top10)))

if (length(platoon_data) > 0) {
  cat("\n✅ Game logs pulled successfully!\n")
  cat("   Next step: Join pitcher handedness data to calculate splits\n")
  
  # Save raw game logs
  saveRDS(platoon_data, "data/milb_game_logs_top10.rds")
  cat("\n💾 Saved to: data/milb_game_logs_top10.rds\n")
  
  # Show sample of what we got
  cat("\n📋 Sample game log structure (first player):\n")
  first_player <- names(platoon_data)[1]
  print(head(platoon_data[[first_player]], 3))
  print(names(platoon_data[[first_player]]))
  
} else {
  cat("\n⚠️  No data pulled - check if baseballr::fg_milb_batter_game_logs() is working\n")
  cat("   Alternative: Manual FanGraphs scraping or different approach needed\n")
}

cat("\n")
