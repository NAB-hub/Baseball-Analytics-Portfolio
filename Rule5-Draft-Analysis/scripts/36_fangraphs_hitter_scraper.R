# 36_fangraphs_hitter_scraper.R
# Scrape MiLB hitting stats directly from FanGraphs player pages
# Same approach that worked for pitcher data

library(dplyr)
library(rvest)
library(httr)
library(tidyr)

cat("\n🔥 FANGRAPHS HITTER DATA SCRAPER 🔥\n")
cat("   Downloading 2021-2025 MiLB stats for R5 benchmark players\n")
cat("   LETS FUCKING GO - 1 MORE MILE!\n\n")

# Load R5 returned players
hitters <- read.csv("output/position_players_filtered.csv", stringsAsFactors = FALSE)

r5_returned <- hitters %>%
  filter(grepl("Rule 5|R5 return", HOW.ACQUIRED, ignore.case = TRUE)) %>%
  select(PLAYER, playerId, position_bucket) %>%
  distinct()

cat("🎯 Target:", nrow(r5_returned), "R5 returned players\n")
cat("   Years: 2021-2025 (last 4 seasons)\n\n")

# Scrape function for individual player
scrape_milb_hitting <- function(player_id, player_name) {
  
  # Create name slug (lowercase, replace spaces with dashes)
  name_slug <- tolower(gsub(" ", "-", player_name))
  
  # FanGraphs URL format: /players/name-slug/playerid/stats?position=OF
  base_url <- paste0("https://www.fangraphs.com/players/", name_slug, "/", player_id, "/stats?position=OF")
  
  cat("   ", player_name, "(", player_id, ")...")
  
  all_data <- data.frame()
  
  tryCatch({
    
    # Read the page
    response <- GET(base_url, 
                    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
    
    if (status_code(response) != 200) {
      cat(" ❌ HTTP", status_code(response), "\n")
      return(NULL)
    }
    
    page <- read_html(content(response, "text"))
    
    # Find all tables on the page
    tables <- page %>% html_nodes("table")
    
    if (length(tables) == 0) {
      cat(" ⚠️  No tables found\n")
      return(NULL)
    }
    
    # Look for MiLB batting table (usually has columns like Season, Team, Level, PA, etc.)
    for (table in tables) {
      
      # Parse table with unique column names
      df <- tryCatch({
        table %>% html_table(fill = TRUE) %>% 
          setNames(make.unique(names(.)))
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(df)) next
      if (ncol(df) < 5) next  # Skip small tables
      
      # Check if this looks like MiLB batting stats
      headers <- tolower(names(df))
      
      if (any(grepl("season", headers)) && 
          any(grepl("level", headers)) && 
          any(grepl("pa|ab", headers))) {
        
        # This is likely the batting table!
        # Filter for MiLB levels only (exclude MLB)
        if ("Level" %in% names(df)) {
          df <- df %>% filter(Level %in% c("AAA", "AA", "A+", "A", "A-", "Rk"))
        }
        
        # Filter for 2021-2025
        if ("Season" %in% names(df)) {
          df <- df %>% filter(Season >= 2021 & Season <= 2025)
        }
        
        if (nrow(df) > 0) {
          df$playerId <- player_id
          df$player_name <- player_name
          all_data <- bind_rows(all_data, df)
        }
      }
    }
    
    if (nrow(all_data) > 0) {
      cat(" ✅", nrow(all_data), "seasons\n")
    } else {
      cat(" ⚠️  No MiLB data 2021-2025\n")
    }
    
  }, error = function(e) {
    cat(" ❌ Error:", e$message, "\n")
  })
  
  Sys.sleep(2)  # Rate limiting - be nice to FanGraphs
  
  return(all_data)
}

# Scrape all players
cat("🌐 Starting web scrape...\n\n")

all_hitting_stats <- data.frame()
success_count <- 0

for (i in 1:nrow(r5_returned)) {
  
  player <- r5_returned[i, ]
  
  cat(i, "/", nrow(r5_returned), ":")
  
  stats <- scrape_milb_hitting(player$playerId, player$PLAYER)
  
  if (!is.null(stats) && nrow(stats) > 0) {
    stats$position_bucket <- player$position_bucket
    all_hitting_stats <- bind_rows(all_hitting_stats, stats)
    success_count <- success_count + 1
  }
  
  # Save progress every 10 players
  if (i %% 10 == 0) {
    write.csv(all_hitting_stats, "output/r5_hitting_stats_PARTIAL.csv", row.names = FALSE)
    cat("\n   💾 Progress saved:", success_count, "players with data\n\n")
  }
}

# Final save
write.csv(all_hitting_stats, "data/r5_benchmark_hitting_2021_2025.csv", row.names = FALSE)

cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("  ✅ SCRAPING COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  Players attempted:", nrow(r5_returned), "\n")
cat("  Players with data:", success_count, "\n")
cat("  Total season records:", nrow(all_hitting_stats), "\n")
cat("  Output: data/r5_benchmark_hitting_2021_2025.csv\n\n")

# Preview
if (nrow(all_hitting_stats) > 0) {
  cat("📊 Data preview:\n")
  print(head(all_hitting_stats %>% 
               select(player_name, Season, Level, Team, PA, AVG, OBP, SLG), 15))
  
  cat("\n📈 Season distribution:\n")
  print(all_hitting_stats %>% count(Season))
  
  cat("\n📍 Level distribution:\n")
  print(all_hitting_stats %>% count(Level))
}

cat("\n🏁 UNSINKABLE! Data downloaded - ready for benchmark analysis!\n")
