# 38_scrape_all_hitters.R
# Scrape MiLB hitting stats for ALL 294 AA/AAA position players
# Then filter using R5 benchmark thresholds

library(dplyr)
library(rvest)
library(httr)
library(tidyr)

cat("\n🔥 SCRAPING ALL 294 POSITION PLAYERS 🔥\n")
cat("   Target: AA/AAA hitters from filtered list\n")
cat("   Years: 2023-2025 (recent performance)\n\n")

# Load filtered position players (AA/AAA only)
all_hitters <- read.csv("output/position_players_filtered.csv", stringsAsFactors = FALSE)

cat("🎯 Target:", nrow(all_hitters), "position players at AA/AAA\n\n")

# Scrape function (same as R5 scraper)
scrape_milb_hitting <- function(player_id, player_name) {
  
  name_slug <- tolower(gsub(" ", "-", player_name))
  base_url <- paste0("https://www.fangraphs.com/players/", name_slug, "/", player_id, "/stats?position=OF")
  
  cat("   ", player_name, "(", player_id, ")...")
  
  all_data <- data.frame()
  
  tryCatch({
    
    response <- GET(base_url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
    
    if (status_code(response) != 200) {
      cat(" ❌ HTTP", status_code(response), "\n")
      return(NULL)
    }
    
    page <- read_html(content(response, "text"))
    tables <- page %>% html_nodes("table")
    
    if (length(tables) == 0) {
      cat(" ⚠️  No tables\n")
      return(NULL)
    }
    
    for (table in tables) {
      df <- tryCatch({
        table %>% html_table(fill = TRUE) %>% 
          setNames(make.unique(names(.)))
      }, error = function(e) return(NULL))
      
      if (is.null(df) || ncol(df) < 5) next
      
      headers <- tolower(names(df))
      
      if (any(grepl("season", headers)) && 
          any(grepl("level", headers)) && 
          any(grepl("pa|ab", headers))) {
        
        if ("Level" %in% names(df)) {
          df <- df %>% filter(Level %in% c("AAA", "AA", "A+", "A", "A-", "Rk"))
        }
        
        if ("Season" %in% names(df)) {
          df <- df %>% filter(Season >= 2023 & Season <= 2025)
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
      cat(" ⚠️  No data\n")
    }
    
  }, error = function(e) {
    cat(" ❌", e$message, "\n")
  })
  
  Sys.sleep(1.5)  # Rate limiting
  
  return(all_data)
}

# Scrape all players
cat("🌐 Starting web scrape (this will take ~7-8 minutes)...\n\n")

all_hitting_stats <- data.frame()
success_count <- 0
start_time <- Sys.time()

for (i in 1:nrow(all_hitters)) {
  
  player <- all_hitters[i, ]
  
  cat(i, "/", nrow(all_hitters), ":")
  
  stats <- scrape_milb_hitting(player$playerId, player$PLAYER)
  
  if (!is.null(stats) && nrow(stats) > 0) {
    stats$position_bucket <- player$position_bucket
    stats$org <- player$Team
    all_hitting_stats <- bind_rows(all_hitting_stats, stats)
    success_count <- success_count + 1
  }
  
  # Save progress every 25 players
  if (i %% 25 == 0) {
    write.csv(all_hitting_stats, "output/all_hitters_stats_PARTIAL.csv", row.names = FALSE)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    cat("\n   💾 Progress saved:", success_count, "players |", round(elapsed, 1), "min elapsed\n\n")
  }
}

elapsed_total <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))

# Final save
write.csv(all_hitting_stats, "data/all_hitters_milb_2023_2025.csv", row.names = FALSE)

cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("  ✅ SCRAPING COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  Time elapsed:", round(elapsed_total, 1), "minutes\n")
cat("  Players attempted:", nrow(all_hitters), "\n")
cat("  Players with data:", success_count, "\n")
cat("  Total season records:", nrow(all_hitting_stats), "\n")
cat("  Output: data/all_hitters_milb_2023_2025.csv\n\n")

# Preview
if (nrow(all_hitting_stats) > 0) {
  cat("📊 Data preview:\n")
  print(head(all_hitting_stats %>% 
               select(player_name, Season, Level, Team, org, position_bucket), 10))
  
  cat("\n📈 Season distribution:\n")
  print(all_hitting_stats %>% count(Season))
  
  cat("\n📍 Level distribution:\n")
  print(all_hitting_stats %>% count(Level))
}

cat("\n🏁 NEXT: Run analysis script to filter using R5 benchmarks!\n")
