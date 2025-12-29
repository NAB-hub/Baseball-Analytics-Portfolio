# =============================================================================
# 42_enhanced_html_with_injuries.R
# Generate front-office ready HTML with injury flags and classifications
# No fluff - data-backed only
# =============================================================================

library(dplyr)

cat("📊 Building Enhanced HTML Report...\n\n")

# Load data - must run script 41 first
if(!file.exists("output/overall_rankings_with_injuries.csv")) {
  stop("❌ Run script 41 first to merge injury data")
}

overall <- read.csv("output/overall_rankings_with_injuries.csv", stringsAsFactors = FALSE)
player_stats <- read.csv("data/player_career_stats.csv", stringsAsFactors = FALSE)

# Get top 25 with full metrics
top25_full <- overall %>%
  left_join(player_stats %>% select(playerid, career_gb_pct, career_hr_9), 
            by = "playerid") %>%
  filter(overall_rank <= 25) %>%
  mutate(
    career_gb_pct = ifelse(is.na(career_gb_pct) | is.infinite(career_gb_pct), NA, career_gb_pct),
    career_hr_9 = ifelse(is.na(career_hr_9) | is.infinite(career_hr_9), NA, career_hr_9)
  ) %>%
  arrange(overall_rank)

# Add draft classification
top25_full <- top25_full %>%
  mutate(
    draft_class = case_when(
      latest_level == "AAA" & latest_age >= 24 & career_fip < 4.0 & injury_risk %in% c("NONE", "MODERATE") ~ "MLB Ready",
      latest_level == "AAA" & latest_age >= 25 & injury_risk %in% c("NONE", "MODERATE") ~ "MLB Ready",
      latest_age <= 23 & career_k9 >= 10.5 & injury_risk %in% c("NONE", "MODERATE") ~ "High Upside",
      injury_risk %in% c("CRITICAL", "HIGH") ~ "Medical Review",
      total_ip < 80 ~ "Monitor - Small Sample",
      TRUE ~ "Monitor"
    ),
    
    mlb_role = case_when(
      primary_role == "Multi-Inning" ~ "Bulk/Follower (3-4 IP)",
      primary_role == "One-Inning" & career_k9 >= 11 ~ "High-Leverage (7-9)",
      primary_role == "One-Inning" ~ "Middle Relief (6-8)",
      primary_role == "Hybrid" ~ "Flexible (1-3 IP)",
      TRUE ~ "TBD"
    )
  )

# Summary stats
scarcity_pct <- round(sum(overall$overall_grade %in% c("A", "B+")) / nrow(overall) * 100, 1)
top25_injuries <- top25_full %>% 
  filter(injury_risk %in% c("CRITICAL", "HIGH")) %>% 
  nrow()

cat("📈 Report Stats:\n")
cat(sprintf("   - B+ or better: %d/%d (%.1f%%)\n", 
            sum(overall$overall_grade %in% c("A", "B+")), nrow(overall), scarcity_pct))
cat(sprintf("   - Top 25 with injury concerns: %d\n", top25_injuries))

# Calculate key stats for HTML
total_b_plus <- sum(overall$overall_grade %in% c("A", "B+"))
total_pitchers <- nrow(overall)

# Start HTML
html <- '<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Rule 5 Relief Pitcher Analysis - Enhanced</title>
    <style>
        body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Arial, sans-serif; 
               max-width: 1400px; margin: 20px auto; padding: 20px; background: #fff; color: #222; line-height: 1.6; }
        h1 { font-size: 28px; border-bottom: 3px solid #0d47a1; padding-bottom: 10px; margin-bottom: 5px; color: #0d47a1; }
        h2 { font-size: 22px; margin-top: 30px; border-left: 4px solid #0d47a1; padding-left: 10px; }
        h3 { font-size: 18px; margin-top: 20px; color: #333; }
        .subtitle { color: #666; font-size: 14px; margin-bottom: 30px; }
        
        .exec-box { background: #e3f2fd; border: 2px solid #0d47a1; padding: 20px; margin: 20px 0; border-radius: 4px; }
        .exec-box h3 { margin-top: 0; color: #0d47a1; }
        
        .injury-critical { background: #ffebee; border-left: 4px solid #c62828; padding: 15px; margin: 15px 0; }
        .injury-high { background: #fff3e0; border-left: 4px solid #ef6c00; padding: 15px; margin: 15px 0; }
        
        table { width: 100%; border-collapse: collapse; font-size: 13px; margin: 20px 0; }
        th { background: #0d47a1; color: #fff; padding: 10px 6px; text-align: left; cursor: pointer; }
        th:hover { background: #1565c0; }
        td { padding: 8px 6px; border-bottom: 1px solid #ddd; }
        tr:hover { background: #f5f5f5; }
        .num { text-align: right; }
        
        .badge-critical { background: #c62828; color: white; padding: 2px 8px; border-radius: 3px; font-size: 11px; font-weight: bold; }
        .badge-high { background: #ef6c00; color: white; padding: 2px 8px; border-radius: 3px; font-size: 11px; }
        .badge-moderate { background: #ffa726; color: white; padding: 2px 8px; border-radius: 3px; font-size: 11px; }
        .badge-mlb { background: #2e7d32; color: white; padding: 2px 8px; border-radius: 3px; font-size: 11px; }
        .badge-upside { background: #0277bd; color: white; padding: 2px 8px; border-radius: 3px; font-size: 11px; }
        
        ul { margin-left: 20px; }
        li { margin-bottom: 8px; }
    </style>
</head>
<body>
    <h1>Rule 5 Relief Pitcher Analysis</h1>
    <div class="subtitle">527 eligible pitchers | 1,774 MiLB seasons (2021-2025) | FanGraphs data</div>
    
    <div class="exec-box">
        <h3>Executive Summary</h3>
        <p><strong>Problem:</strong> Rule 5 draft boards often prioritize ceiling over immediate readiness, leading to roster waste and waiver returns.</p>
        <p><strong>Solution:</strong> This analysis ranks 527 Rule 5-eligible relievers using performance (FIP, K/9, BB/9), context (age/level fit), and projection, with explicit injury flags and role classifications.</p>
        <p><strong>Key Finding:</strong> Only '

html <- paste0(html, sprintf('%d of %d pitchers (%.1f%%) grade B+ or better', 
            total_b_plus, total_pitchers, scarcity_pct), ', making efficient board construction critical.</p>
        <ul>
            <li><strong>Top 3:</strong> Yordanny Monegro (⚠️ Tommy John 6/2025), Evan Reifert, Nick Altermatt</li>
            <li><strong>Injury Concerns:</strong> ')

html <- paste0(html, sprintf('%d of top 25 flagged CRITICAL/HIGH', top25_injuries), '</li>
            <li><strong>LHP Options:</strong> 5 in top 25 (scarcity value)</li>
        </ul>
    </div>')

# INJURY WARNING SECTION
critical_top25 <- top25_full %>% filter(injury_risk == "CRITICAL")
high_top25 <- top25_full %>% filter(injury_risk == "HIGH")

if(nrow(critical_top25) > 0 || nrow(high_top25) > 0) {
  html <- paste0(html, '
    
    <h2>⚠️ Injury Risk Flags (Top 25)</h2>
    <p><strong>Critical Finding:</strong> Medical review required before selection. Cross-reference with team physicians.</p>')
  
  if(nrow(critical_top25) > 0) {
    html <- paste0(html, '
    <div class="injury-critical">
        <h3>🚨 CRITICAL - Do Not Draft Without Medical Clearance</h3>
        <ul>')
    
    for(i in 1:nrow(critical_top25)) {
      row <- critical_top25[i,]
      html <- paste0(html, sprintf('
            <li><strong>#%d %s</strong> (%s, %dyo, %s): %s</li>',
        row$overall_rank, row$player_name, row$pitcher_hand, row$latest_age, row$latest_level,
        substr(row$injury_history, 1, 100)))
    }
    html <- paste0(html, '
        </ul>
    </div>')
  }
  
  if(nrow(high_top25) > 0) {
    html <- paste0(html, '
    <div class="injury-high">
        <h3>⚠️ HIGH RISK - Medical Staff Review Recommended</h3>
        <ul>')
    
    for(i in 1:nrow(high_top25)) {
      row <- high_top25[i,]
      html <- paste0(html, sprintf('
            <li><strong>#%d %s</strong> (%s, %dyo, %s): %s</li>',
        row$overall_rank, row$player_name, row$pitcher_hand, row$latest_age, row$latest_level,
        substr(row$injury_history, 1, 100)))
    }
    html <- paste0(html, '
        </ul>
    </div>')
  }
}

# TOP 25 TABLE WITH ALL FLAGS
html <- paste0(html, sprintf('
    
    <h2>Top 25 Rankings - Complete Data</h2>
    <p>Click column headers to sort. %d players flagged for injury concerns.</p>
    
    <table id="rankingsTable">
        <thead>
            <tr>
                <th onclick="sortTable(0)">Rank</th>
                <th onclick="sortTable(1)">Player</th>
                <th onclick="sortTable(2)">Hand</th>
                <th onclick="sortTable(3)">Age</th>
                <th onclick="sortTable(4)">Lvl</th>
                <th onclick="sortTable(5)">IP</th>
                <th onclick="sortTable(6)">FIP</th>
                <th onclick="sortTable(7)">K/9</th>
                <th onclick="sortTable(8)">BB/9</th>
                <th onclick="sortTable(9)">GB%%</th>
                <th onclick="sortTable(10)">Score</th>
                <th onclick="sortTable(11)">Grade</th>
                <th onclick="sortTable(12)">Injury</th>
                <th onclick="sortTable(13)">Class</th>
                <th onclick="sortTable(14)">Role</th>
            </tr>
        </thead>
        <tbody>', top25_injuries))

for(i in 1:nrow(top25_full)) {
  row <- top25_full[i,]
  
  injury_badge <- case_when(
    row$injury_risk == "CRITICAL" ~ '<span class="badge-critical">CRITICAL</span>',
    row$injury_risk == "HIGH" ~ '<span class="badge-high">HIGH</span>',
    row$injury_risk == "MODERATE" ~ '<span class="badge-moderate">MOD</span>',
    TRUE ~ "—"
  )
  
  class_badge <- case_when(
    row$draft_class == "MLB Ready" ~ '<span class="badge-mlb">MLB Ready</span>',
    row$draft_class == "High Upside" ~ '<span class="badge-upside">Upside</span>',
    row$draft_class == "Medical Review" ~ '<span class="badge-critical">Med Review</span>',
    TRUE ~ row$draft_class
  )
  
  html <- paste0(html, sprintf('
            <tr>
                <td class="num">%d</td>
                <td><strong>%s</strong></td>
                <td>%s</td>
                <td class="num">%d</td>
                <td>%s</td>
                <td class="num">%.1f</td>
                <td class="num">%.2f</td>
                <td class="num">%.1f</td>
                <td class="num">%.1f</td>
                <td class="num">%s</td>
                <td class="num">%.1f</td>
                <td><strong>%s</strong></td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
            </tr>',
    row$overall_rank,
    row$player_name,
    ifelse(is.na(row$pitcher_hand), "R", row$pitcher_hand),
    row$latest_age,
    row$latest_level,
    row$total_ip,
    row$career_fip,
    row$career_k9,
    row$career_bb9,
    ifelse(is.na(row$career_gb_pct), "—", sprintf("%.1f", row$career_gb_pct)),
    row$composite_score,
    row$overall_grade,
    injury_badge,
    class_badge,
    row$mlb_role
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
            var aVal = a.cells[col].textContent.trim().replace(/[^0-9.-]/g, "");
            var bVal = b.cells[col].textContent.trim().replace(/[^0-9.-]/g, "");
            
            if (!isNaN(aVal) && !isNaN(bVal)) {
                return isAsc ? parseFloat(bVal) - parseFloat(aVal) : parseFloat(aVal) - parseFloat(bVal);
            }
            
            return isAsc ? b.cells[col].textContent.localeCompare(a.cells[col].textContent) 
                         : a.cells[col].textContent.localeCompare(b.cells[col].textContent);
        });
        
        rows.forEach(row => table.tBodies[0].appendChild(row));
        table.setAttribute("data-sort-col", col);
        table.setAttribute("data-sort-dir", isAsc ? "desc" : "asc");
    }
    </script>')

# STRATEGIC RECOMMENDATIONS
html <- paste0(html, '
    
    <h2>Strategic Recommendations</h2>
    
    <h3>Immediate Action Items</h3>
    <ul>
        <li><strong>Medical Review:</strong> Verify Monegro (Tommy John), Simon (elbow surgery) status with physicians</li>
        <li><strong>LHP Priority:</strong> Walling, Strickland, Smeltz, Garnett, Och (5 in top 25)</li>
        <li><strong>MLB-Ready Arms:</strong> Reifert (AAA, 26), Pushard (AAA, 27), Birdsell (AAA, 25)</li>
        <li><strong>High Upside Bets:</strong> Altermatt, Coleman, Dabovich (all A grade, under 25)</li>
    </ul>
    
    <h3>Risk Mitigation</h3>
    <ul>
        <li>Cross-reference injury flags with spring training reports and velocity data</li>
        <li>Prioritize pitchers with 100+ IP and consistent multi-season performance</li>
        <li>For A+ arms (9 in top 25), verify they can handle jump to MLB workload</li>
    </ul>')

# METHODOLOGY
html <- paste0(html, '
    
    <h2>Methodology</h2>
    
    <h3>Scoring Framework</h3>
    <p><strong>Composite Score</strong> = (Tier 1 × 50%) + (Tier 2 × 30%) + (Tier 3 × 20%)</p>
    <ul>
        <li><strong>Tier 1 (Performance):</strong> FIP 40%, K/9 25%, BB/9 20%, K-BB 15%</li>
        <li><strong>Tier 2 (Context):</strong> Age/Level 25%, GB% 25%, HR/9 15%, Consistency 15%, IP 10%, Level 10%</li>
        <li><strong>Tier 3 (Projection):</strong> Stuff proxy, MLB readiness, peak performance</li>
    </ul>
    
    <h3>Grade Scale</h3>
    <ul>
        <li><strong>A (75+):</strong> Elite - immediate MLB impact potential</li>
        <li><strong>B+ (65-74):</strong> Strong - likely MLB contributor</li>
        <li><strong>B (55-64):</strong> Solid - MLB potential with development</li>
        <li><strong>C+ (45-54):</strong> Fringe - organizational depth</li>
    </ul>
    
    <h3>Injury Risk Classification</h3>
    <ul>
        <li><strong>CRITICAL:</strong> Tommy John, season-ending surgery, UCL tears</li>
        <li><strong>HIGH:</strong> Multiple IL stints, elbow issues, stress fractures, 60-day IL</li>
        <li><strong>MODERATE:</strong> Single 7-day IL, shoulder/arm strains</li>
    </ul>
    
    <h3>Draft Classification</h3>
    <ul>
        <li><strong>MLB Ready:</strong> AAA experience, age 24+, FIP < 4.0, no critical injuries</li>
        <li><strong>High Upside:</strong> Age ≤23, K/9 ≥10.5, projection-driven</li>
        <li><strong>Medical Review:</strong> Critical/high injury risk - requires clearance</li>
        <li><strong>Monitor:</strong> Small sample (<80 IP) or age/level concerns</li>
    </ul>
    
    <h2>Limitations</h2>
    <ul>
        <li>No historical Rule 5 pick validation (backtest against actual outcomes needed)</li>
        <li>No platoon splits (vsL/vsR data not in FanGraphs MiLB)</li>
        <li>Park factors flagged (IL vs PCL) but not statistically adjusted</li>
        <li>Injury data from public sources only (team medical records unavailable)</li>
        <li>Weights based on industry standards, not optimized via ML</li>
    </ul>
    
    <p style="margin-top: 40px; padding-top: 20px; border-top: 1px solid #ccc; color: #666; font-size: 13px;">
    Analysis: December 2025 | Data: FanGraphs MiLB (2021-2025) | 527 pitchers, 1,774 season records | Injury research: Public sources
    </p>
</body>
</html>')

# Write file
writeLines(html, "reports/Rule5_Enhanced_Report.html")

cat("\n✅ ENHANCED HTML GENERATED!\n")
cat("   📄 File: reports/Rule5_Enhanced_Report.html\n")
cat("   ✓ Injury flags added\n")
cat("   ✓ Draft classifications included\n")
cat("   ✓ MLB role translations\n")
cat("   ✓ Strategic recommendations\n")
cat("   ✓ No fluff - data backed only\n\n")
