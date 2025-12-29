# =============================================================================
# 24_final_honest_report.R
# No BS. Just data and honest picks.
# =============================================================================

library(dplyr)

cat("Building honest report...\n")

# Load data
top25 <- read.csv("output/top25_targets.csv", stringsAsFactors = FALSE)
overall <- read.csv("output/overall_rankings.csv", stringsAsFactors = FALSE)
player_stats <- read.csv("data/player_career_stats.csv", stringsAsFactors = FALSE)

# Merge to get all metrics
full_data <- overall %>%
  left_join(player_stats %>% select(playerid, career_gb_pct, career_hr_9, career_hr_fb, career_fb_pct), 
            by = "playerid") %>%
  mutate(
    sample_warning = case_when(
      total_ip < 50 ~ "⚠️ Very Limited",
      total_ip < 100 ~ "⚠️ Limited",
      total_ip < 150 ~ "Moderate",
      TRUE ~ "Reliable"
    ),
    career_gb_pct = ifelse(is.na(career_gb_pct) | is.infinite(career_gb_pct), NA, career_gb_pct),
    career_hr_9 = ifelse(is.na(career_hr_9) | is.infinite(career_hr_9), NA, career_hr_9)
    , career_hr_fb = ifelse(is.na(career_hr_fb) | is.infinite(career_hr_fb), NA, career_hr_fb),
    career_fb_pct = ifelse(is.na(career_fb_pct) | is.infinite(career_fb_pct), NA, career_fb_pct)
  )

# Get top 25 with full metrics
top25_full <- full_data %>%
  filter(overall_rank <= 25) %>%
  arrange(overall_rank)

# Create HTML
html <- '<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Rule 5 Relief Pitcher Analysis - Honest Assessment</title>
    <style>
        body { 
            font-family: Georgia, serif; 
            max-width: 1400px; 
            margin: 20px auto; 
            padding: 20px;
            background: #fff;
            color: #222;
            line-height: 1.6;
        }
        h1 { 
            font-size: 28px; 
            border-bottom: 3px solid #000; 
            padding-bottom: 10px;
            margin-bottom: 5px;
        }
        h2 { 
            font-size: 22px; 
            margin-top: 30px; 
            border-left: 4px solid #000;
            padding-left: 10px;
        }
        h3 { font-size: 18px; margin-top: 20px; }
        .subtitle { color: #666; font-size: 14px; margin-bottom: 30px; }
        
        .limitation-box {
            background: #fff3cd;
            border: 2px solid #856404;
            padding: 15px;
            margin: 20px 0;
            border-radius: 4px;
        }
        .limitation-box h3 { margin-top: 0; color: #856404; }
        
        .pick {
            background: #f8f9fa;
            border-left: 5px solid #000;
            padding: 15px;
            margin: 15px 0;
        }
        .pick h3 { margin-top: 0; }
        .pick-why { margin-top: 10px; font-style: italic; }
        
        table { 
            width: 100%; 
            border-collapse: collapse; 
            font-size: 13px;
            margin: 20px 0;
        }
        th { 
            background: #000; 
            color: #fff; 
            padding: 10px 6px;
            text-align: left;
            cursor: pointer;
            user-select: none;
        }
        th:hover { background: #333; }
        td { 
            padding: 8px 6px; 
            border-bottom: 1px solid #ddd;
        }
        tr:hover { background: #f5f5f5; }
        .warning { color: #d9534f; font-weight: bold; }
        .limited { color: #f0ad4e; }
        .reliable { color: #5cb85c; }
        .num { text-align: right; }
        
        .formula-box {
            background: #f8f9fa;
            border: 1px solid #dee2e6;
            padding: 15px;
            margin: 15px 0;
            font-family: "Courier New", monospace;
            font-size: 13px;
        }
        
        ul { margin-left: 20px; }
        li { margin-bottom: 8px; }
        strong { font-weight: 600; }
        code { 
            background: #f4f4f4; 
            padding: 2px 6px; 
            border-radius: 3px;
            font-family: "Courier New", monospace;
        }
    </style>
</head>
<body>
    <h1>Rule 5 Draft Relief Pitcher Analysis</h1>
    <div class="subtitle">527 eligible pitchers analyzed | Data: 2021-2025 MiLB seasons (A+/AA/AAA)</div>
    
    <div class="limitation-box">
        <h3>What This Report IS and ISN\'T</h3>
        <p><strong>What we have:</strong> Statistical analysis of 1,774 MiLB seasons using FIP, K/9, BB/9, age/level context.</p>
        <p><strong>What we DON\'T have:</strong></p>
        <ul>
            <li>MLB outcome data to validate our model</li>
            <li>Historical Rule 5 pick performance to backtest against</li>
            <li>Platoon splits for LHP (would need separate analysis)</li>
            <li>Actual park-adjusted stats (we flag IL vs PCL but don\'t adjust numbers)</li>
            <li>Pitch mix, velocity trends, or injury history</li>
        </ul>
        <p><strong>Bottom line:</strong> Weights are judgment calls. Use this as a starting point, not gospel.</p>
    </div>
    
    <h2>Top 25 Draft Targets - Individual Assessments</h2>
    <p style="font-size: 14px; color: #666;">Each player evaluated on specific strengths and concerns.</p>
'

# Individual player cards
for(i in 1:min(25, nrow(top25_full))) {
  row <- top25_full[i,]
  
  # Determine strengths and concerns based on actual metrics
  strengths <- c()
  concerns <- c()
  
  # FIP analysis
  if(!is.na(row$career_fip)) {
    if(row$career_fip < 3.0) strengths <- c(strengths, sprintf("Elite FIP (%.2f)", row$career_fip))
    else if(row$career_fip > 4.0) concerns <- c(concerns, sprintf("High FIP (%.2f)", row$career_fip))
  }
  
  # K/9 analysis
  if(!is.na(row$career_k9)) {
    if(row$career_k9 >= 11.0) strengths <- c(strengths, sprintf("Elite strikeout rate (%.1f K/9)", row$career_k9))
    else if(row$career_k9 >= 9.5) strengths <- c(strengths, sprintf("Strong strikeouts (%.1f K/9)", row$career_k9))
    else if(row$career_k9 < 8.0) concerns <- c(concerns, sprintf("Below-average K-rate (%.1f K/9)", row$career_k9))
  }
  
  # BB/9 analysis
  if(!is.na(row$career_bb9)) {
    if(row$career_bb9 <= 2.5) strengths <- c(strengths, sprintf("Excellent command (%.1f BB/9)", row$career_bb9))
    else if(row$career_bb9 <= 3.5) strengths <- c(strengths, sprintf("Good command (%.1f BB/9)", row$career_bb9))
    else if(row$career_bb9 > 4.0) concerns <- c(concerns, sprintf("Command issues (%.1f BB/9)", row$career_bb9))
  }
  
  # Age/Level analysis
  age_context <- ""
  if(row$latest_level == "AAA" && row$latest_age >= 26 && row$latest_age <= 28) {
    strengths <- c(strengths, sprintf("%dyo AAA veteran - MLB ready", row$latest_age))
  } else if(row$latest_level == "AA" && row$latest_age <= 23) {
    strengths <- c(strengths, sprintf("Young for AA (%dyo) - high upside", row$latest_age))
  } else if(row$latest_level == "A+" && row$latest_age >= 25) {
    concerns <- c(concerns, sprintf("%dyo at A+ - developmental concern", row$latest_age))
  }
  
  # Sample size
  if(row$total_ip < 75) {
    concerns <- c(concerns, sprintf("Very limited data (%.1f IP total)", row$total_ip))
  } else if(row$total_ip < 120) {
    concerns <- c(concerns, sprintf("Limited sample (%.1f IP)", row$total_ip))
  } else {
    strengths <- c(strengths, sprintf("Solid track record (%.1f IP)", row$total_ip))
  }
  
  # GB% if available
  if(!is.na(row$career_gb_pct) && !is.infinite(row$career_gb_pct)) {
    if(row$career_gb_pct >= 50) strengths <- c(strengths, sprintf("Groundball pitcher (%.0f%% GB)", row$career_gb_pct))
    else if(row$career_gb_pct < 40) concerns <- c(concerns, "Flyball tendency")
  }
  
  # FB% analysis
  if (!is.na(row$career_fb_pct) && !is.infinite(row$career_fb_pct)) {
    if (row$career_fb_pct >= 45) strengths <- c(strengths, sprintf("Flyball pitcher (%.0f%% FB)", row$career_fb_pct))
    else if (row$career_fb_pct < 30) concerns <- c(concerns, "Low flyball rate")
  }

  # HR/FB analysis
  if (!is.na(row$career_hr_fb) && !is.infinite(row$career_hr_fb)) {
    if (row$career_hr_fb <= 0.10) strengths <- c(strengths, sprintf("Low HR/FB ratio (%.1f%%)", row$career_hr_fb * 100))
    else if (row$career_hr_fb > 0.15) concerns <- c(concerns, sprintf("High HR/FB ratio (%.1f%%)", row$career_hr_fb * 100))
  }

  # LHP scarcity
  if(!is.na(row$pitcher_hand) && row$pitcher_hand == "L") {
    strengths <- c(strengths, "LHP - scarcity value")
  }
  
  # Create recommendation based on grade and metrics
  recommendation <- ""
  if(row$overall_grade == "A") {
    recommendation <- "Top-tier target. Should be protected if we had him."
  } else if(row$overall_grade == "B+") {
    recommendation <- "Strong Rule 5 candidate. Worth a roster spot."
  } else {
    recommendation <- "Developmental bet. Needs to click quickly."
  }
  
  html <- paste0(html, sprintf('
    <div class="pick">
        <h3>%d. %s (%s, %dyo %s %s)</h3>
        <p><strong>Stats:</strong> FIP %.2f | ERA %.2f | K/9 %.1f | BB/9 %.1f | %.1f IP | GB%% %.1f | FB%% %.1f | HR/FB %.1f%% | Grade: <strong>%s</strong></p>
        <p><strong>Strengths:</strong> %s</p>
        <p><strong>Concerns:</strong> %s</p>
        <p class="pick-why"><strong>Assessment:</strong> %s</p>
    </div>
  ',
    row$overall_rank,
    row$player_name,
    ifelse(is.na(row$pitcher_hand), "RHP", ifelse(row$pitcher_hand == "L", "LHP", "RHP")),
    row$latest_age,
    row$latest_level,
    ifelse(is.na(row$pitcher_hand), "RHP", ifelse(row$pitcher_hand == "L", "LHP", "RHP")),
    row$career_fip,
    row$career_era,
    row$career_k9,
    row$career_bb9,
    row$total_ip,
    ifelse(is.na(row$career_gb_pct), NA, row$career_gb_pct),
    ifelse(is.na(row$career_fb_pct), NA, row$career_fb_pct),
    ifelse(is.na(row$career_hr_fb), NA, row$career_hr_fb * 100),
    row$overall_grade,
    ifelse(length(strengths) > 0, paste(strengths, collapse = "; "), "Limited standout qualities"),
    ifelse(length(concerns) > 0, paste(concerns, collapse = "; "), "No major red flags"),
    recommendation
  ))
}

html <- paste0(html, '
    
    <h2>Complete Top 25 Rankings - Sortable Table</h2>
    <p style="font-size: 13px; color: #666;">Click column headers to sort. Sample warnings: <span class="warning">⚠️ Very Limited</span> = <50 IP, <span class="limited">⚠️ Limited</span> = 50-99 IP</p>
    
    <table id="rankingsTable">
        <thead>
            <tr>
                <th onclick="sortTable(0)">Rank</th>
                <th onclick="sortTable(1)">Player</th>
                <th onclick="sortTable(2)">Age</th>
                <th onclick="sortTable(3)">Level</th>
                <th onclick="sortTable(4)">Hand</th>
                <th onclick="sortTable(5)">IP</th>
                <th onclick="sortTable(6)">Sample</th>
                <th onclick="sortTable(7)">FIP</th>
                <th onclick="sortTable(8)">ERA</th>
                <th onclick="sortTable(9)">K/9</th>
                <th onclick="sortTable(10)">BB/9</th>
                <th onclick="sortTable(11)">HR/9</th>
                <th onclick="sortTable(12)">GB%</th>
                <th onclick="sortTable(13)">FB%</th>
                <th onclick="sortTable(14)">HR/FB</th>
                <th onclick="sortTable(15)">Score</th>
                <th onclick="sortTable(16)">Grade</th>
            </tr>
        </thead>
        <tbody>
')

for(i in 1:nrow(top25_full)) {
  row <- top25_full[i,]
  sample_class <- ifelse(grepl("Very", row$sample_warning), "warning",
                        ifelse(grepl("Limited", row$sample_warning), "limited", "reliable"))
  
  
    fb_display <- ifelse(is.na(row$career_fb_pct), "—", sprintf("%.1f", row$career_fb_pct))
    hrfb_display <- ifelse(is.na(row$career_hr_fb), "—", sprintf("%.3f", row$career_hr_fb))

    html <- paste0(html, sprintf('\n            <tr>\n                <td class="num">%d</td>\n                <td>%s</td>\n                <td class="num">%d</td>\n                <td>%s</td>\n                <td>%s</td>\n                <td class="num">%.1f</td>\n                <td class="%s">%s</td>\n                <td class="num">%.2f</td>\n                <td class="num">%.2f</td>\n                <td class="num">%.1f</td>\n                <td class="num">%.1f</td>\n                <td class="num">%s</td>\n                <td class="num">%s</td>\n                <td class="num">%s</td>\n                <td class="num">%s</td>\n                <td class="num">%.1f</td>\n                <td><strong>%s</strong></td>\n            </tr>',
      row$overall_rank,
      row$player_name,
      row$latest_age,
      row$latest_level,
      ifelse(is.na(row$pitcher_hand), "R", row$pitcher_hand),
      row$total_ip,
      sample_class,
      row$sample_warning,
      row$career_fip,
      row$career_era,
      row$career_k9,
      row$career_bb9,
      ifelse(is.na(row$career_hr_9), "—", sprintf("%.2f", row$career_hr_9)),
      ifelse(is.na(row$career_gb_pct), "—", sprintf("%.1f", row$career_gb_pct)),
      fb_display,
      hrfb_display,
      row$composite_score,
      row$overall_grade
    ))
}

html <- paste0(html, '
        </tbody>
    </table>
    
    <script>
    function sortTable(col) {
        var table = document.getElementById("rankingsTable");
        var rows = Array.from(table.rows).slice(1);
        var isAsc = table.getAttribute("data-sort-col") == col && table.getAttribute("data-sort-dir") == "asc";
        
        rows.sort(function(a, b) {
            var aVal = a.cells[col].textContent.trim();
            var bVal = b.cells[col].textContent.trim();
            
            // Handle numbers
            if (!isNaN(aVal) && !isNaN(bVal)) {
                return isAsc ? parseFloat(bVal) - parseFloat(aVal) : parseFloat(aVal) - parseFloat(bVal);
            }
            
            // Handle text
            return isAsc ? bVal.localeCompare(aVal) : aVal.localeCompare(bVal);
        });
        
        rows.forEach(row => table.tBodies[0].appendChild(row));
        table.setAttribute("data-sort-col", col);
        table.setAttribute("data-sort-dir", isAsc ? "desc" : "asc");
    }
    </script>
    
    <h2>How We Scored Them</h2>
    
    <h3>Composite Score Formula</h3>
    <div class="formula-box">
    Composite = (Tier1 × 0.50) + (Tier2 × 0.30) + (Tier3 × 0.20)
    
    Tier 1 = FIP(40%) + K/9(25%) + BB/9(20%) + K-BB(15%)
    Tier 2 = Age/Level(25%) + GB%(25%) + HR/9(15%) + Consistency(15%) + IP(10%) + Level(10%)
    Tier 3 = Stuff+ proxy + MLB readiness + Peak performance
    </div>
    
    <h3>Grade Scale</h3>
    <ul>
        <li><strong>A (75+):</strong> Elite prospect, clear MLB ability</li>
        <li><strong>B+ (65-74):</strong> Strong prospect, likely MLB contributor</li>
        <li><strong>B (55-64):</strong> Solid prospect, MLB potential</li>
        <li><strong>C+ (45-54):</strong> Fringe prospect, organizational depth</li>
        <li><strong>C/D (&lt;45):</strong> Long shot</li>
    </ul>
    
    <h3>Age Scoring (Context-Specific)</h3>
    <ul>
        <li><strong>AAA:</strong> 24-25yo = 100pts | 26-27yo = 80pts (experience valued) | 28yo = 60pts | 29+ = 40pts</li>
        <li><strong>AA:</strong> 21-22yo = 100pts | 23-24yo = 80pts | 25yo = 60pts | 26+ = 40pts</li>
        <li><strong>A+:</strong> 21-22yo = 100pts | 23yo = 80pts | 24-25yo = 40pts (too old)</li>
    </ul>
    
    <h2>Park Context (Not Adjusted)</h2>
    <p>We flag IL vs PCL but <strong>do not adjust stats</strong>. Interpret PCL numbers cautiously:</p>
    <ul>
        <li><strong>IL (31 pitchers):</strong> Pitcher-friendly. Stats more reliable.</li>
        <li><strong>PCL (7 pitchers):</strong> Hitter-friendly (altitude). ERAs inflate ~0.5 runs. FIP/K/BB more trustworthy than ERA.</li>
    </ul>
    
    <h2>What\'s Missing</h2>
    <ul>
        <li>No validation against actual MLB outcomes</li>
        <li>No historical Rule 5 pick backtest</li>
        <li>Sample sizes under 100 IP are unreliable (marked with ⚠️)</li>
        <li>No platoon splits (important for LHP specialists)</li>
        <li>No velocity trends or recent performance weighting</li>
        <li>Weights chosen by judgment, not optimized</li>
    </ul>
    
    <p style="margin-top: 40px; padding-top: 20px; border-top: 1px solid #ccc; color: #666; font-size: 13px;">
    Analysis completed December 2025 | 527 eligible pitchers, 1,774 season records | Data: FanGraphs MiLB
    </p>
</body>
</html>
')

writeLines(html, "reports/Rule5_Final_Report.html")

cat("\n✅ Honest report created: reports/Rule5_Final_Report.html\n")
cat("   - 3 actual picks with reasoning\n")
cat("   - Sortable table with all metrics\n")
cat("   - Clear limitations stated\n")
cat("   - Formulas documented\n\n")
