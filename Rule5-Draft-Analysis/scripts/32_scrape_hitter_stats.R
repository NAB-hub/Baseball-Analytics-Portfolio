# =============================================================================
# 32_scrape_hitter_stats.R
# Scrape FanGraphs MiLB hitting statistics for Rule 5 eligible position players
# Based on playerId from mastersheet
# =============================================================================

library(dplyr)
library(rvest)
library(httr)

cat("Scraping MiLB hitting statistics from FanGraphs...\n")

# Load position players
hitters <- read.csv("output/position_players_classified.csv", stringsAsFactors = FALSE)

cat(sprintf("Found %d position players to scrape\n", nrow(hitters)))

# Function to scrape hitting stats for a player
scrape_hitter_stats <- function(player_id, player_name) {
  
  url <- sprintf("https://www.fangraphs.com/players/%s/stats?position=OF", player_id)
  
  tryCatch({
    page <- read_html(url)
    
    # This is a placeholder - we'll need to inspect FanGraphs HTML structure
    # Similar to what we did for pitchers
    # Extract tables with hitting stats
    
    cat(sprintf("✓ Scraped: %s (%s)\n", player_name, player_id))
    
    return(data.frame(
      playerid = player_id,
      player_name = player_name,
      scraped = TRUE
    ))
    
  }, error = function(e) {
    cat(sprintf("✗ Failed: %s (%s) - %s\n", player_name, player_id, e$message))
    return(data.frame(
      playerid = player_id,
      player_name = player_name,
      scraped = FALSE
    ))
  })
  
  Sys.sleep(0.5)  # Rate limiting
}

# Test with first 5 players
test_players <- head(hitters, 5)

cat("\nTesting scraper with first 5 players...\n")
for(i in 1:nrow(test_players)) {
  result <- scrape_hitter_stats(test_players$playerId[i], test_players$PLAYER[i])
  print(result)
}

cat("\n⚠️  SCRAPER TEMPLATE CREATED\n")
cat("   Next step: Inspect FanGraphs hitting stats page HTML to extract actual metrics\n")
cat("   Metrics needed: wOBA, wRC+, ISO, BB%, K%, Hard Hit%, GB%, WAR, SB\n")
