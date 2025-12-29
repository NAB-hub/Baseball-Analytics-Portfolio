# =============================================================================
# 43_winter_meetings_onepager.R
# Generate single-page executive summary for Winter Meetings distribution
# Print-ready, data-dense, no BS
# =============================================================================

library(dplyr)

cat("📄 Building Winter Meetings One-Pager...\n\n")

# Load data
overall <- read.csv("output/overall_rankings_with_injuries.csv", stringsAsFactors = FALSE)
player_stats <- read.csv("data/player_career_stats.csv", stringsAsFactors = FALSE)

# Top 10 targets (excluding CRITICAL injuries)
top10_safe <- overall %>%
  filter(injury_risk != "CRITICAL") %>%
  left_join(player_stats %>% select(playerid, career_gb_pct, career_hr_9), by = "playerid") %>%
  arrange(overall_rank) %>%
  head(10) %>%
  mutate(
    mlb_role = case_when(
      primary_role == "Multi-Inning" ~ "Bulk (3-4 IP)",
      primary_role == "One-Inning" & career_k9 >= 11 ~ "High-Lev (7-9)",
      primary_role == "One-Inning" ~ "Mid-Rel (6-8)",
      primary_role == "Hybrid" ~ "Flex (1-3 IP)",
      TRUE ~ "TBD"
    ),
    injury_flag = case_when(
      injury_risk == "HIGH" ~ "⚠️",
      injury_risk == "MODERATE" ~ "◐",
      TRUE ~ ""
    )
  )

# Top 5 LHP
top_lhp <- overall %>%
  filter(pitcher_hand == "L", injury_risk != "CRITICAL") %>%
  left_join(player_stats %>% select(playerid, career_gb_pct), by = "playerid") %>%
  arrange(overall_rank) %>%
  head(5) %>%
  mutate(
    injury_flag = case_when(
      injury_risk == "HIGH" ~ "⚠️",
      injury_risk == "MODERATE" ~ "◐",
      TRUE ~ ""
    )
  )

# Summary stats
total_b_plus <- sum(overall$overall_grade %in% c("A", "B+"))
scarcity_pct <- round(total_b_plus / nrow(overall) * 100, 1)
total_lhp_b_plus <- sum(overall$pitcher_hand == "L" & overall$overall_grade %in% c("A", "B+"))

cat(sprintf("✓ Top 10 (safe): %d players\n", nrow(top10_safe)))
cat(sprintf("✓ Top LHP: %d players\n", nrow(top_lhp)))

# HTML One-Pager
html <- '<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Rule 5 Draft - Winter Meetings Brief</title>
    <style>
        @page { size: letter; margin: 0.5in; }
        @media print {
            body { margin: 0; padding: 0; }
            .no-print { display: none; }
        }
        
        body { 
            font-family: "Calibri", Arial, sans-serif; 
            max-width: 8.5in; 
            margin: 0 auto; 
            padding: 0.5in;
            font-size: 11pt;
            line-height: 1.3;
            background: white;
            color: #000;
        }
        
        .header { 
            border-bottom: 3px solid #000; 
            padding-bottom: 8px; 
            margin-bottom: 15px;
        }
        
        h1 { 
            font-size: 20pt; 
            margin: 0 0 3px 0; 
            font-weight: bold;
        }
        
        .tagline { 
            font-size: 9pt; 
            color: #444; 
            margin: 0;
        }
        
        h2 { 
            font-size: 13pt; 
            margin: 15px 0 8px 0; 
            border-left: 4px solid #000; 
            padding-left: 8px;
            font-weight: bold;
        }
        
        .exec-summary { 
            background: #f0f0f0; 
            border: 2px solid #000; 
            padding: 12px; 
            margin: 12px 0;
            font-size: 10pt;
        }
        
        .exec-summary p { margin: 6px 0; }
        .exec-summary strong { font-weight: bold; }
        
        table { 
            width: 100%; 
            border-collapse: collapse; 
            font-size: 9pt; 
            margin: 10px 0;
        }
        
        th { 
            background: #000; 
            color: #fff; 
            padding: 6px 4px; 
            text-align: left;
            font-weight: bold;
            font-size: 8.5pt;
        }
        
        td { 
            padding: 5px 4px; 
            border-bottom: 1px solid #ccc;
        }
        
        .num { text-align: right; }
        
        .badge-injury { 
            color: #c62828; 
            font-weight: bold;
        }
        
        .grade-a { font-weight: bold; color: #1b5e20; }
        .grade-b-plus { font-weight: bold; color: #0d47a1; }
        
        .footer { 
            margin-top: 15px; 
            padding-top: 8px; 
            border-top: 2px solid #000; 
            font-size: 8pt; 
            color: #555;
        }
        
        .two-col { 
            display: grid; 
            grid-template-columns: 1fr 1fr; 
            gap: 20px; 
            margin: 10px 0;
        }
        
        ul { 
            margin: 5px 0 10px 18px; 
            padding: 0;
            font-size: 10pt;
        }
        
        li { margin-bottom: 4px; }
        
        .compact { margin: 8px 0; }
    </style>
</head>
<body>
    <div class="header">
        <h1>Rule 5 Relief Pitcher Analysis - Winter Meetings Brief</h1>
        <p class="tagline">Top 10 Targets | LHP Options | December 2025 | Data: FanGraphs MiLB 2021-2025</p>
    </div>
    
    <div class="exec-summary">
        <p><strong>Key Finding:</strong> Only ' 

html <- paste0(html, sprintf('%d of 527 Rule 5-eligible relievers (%.1f%%) grade B+ or better', total_b_plus, scarcity_pct), 
               '. Efficient board construction critical.</p>
        <p><strong>Top Risk:</strong> #1 ranked Yordanny Monegro has Tommy John surgery (6/2025) - excluded from recommendations below.</p>
        <p><strong>LHP Scarcity:</strong> Only ', sprintf('%d LHP grade B+ or better', total_lhp_b_plus), ' - prioritize early if team needs southpaw.</p>
    </div>
    
    <h2>Top 10 Targets (Injury-Adjusted)</h2>
    <p class="compact"><strong>Note:</strong> ⚠️ = High injury risk (medical review), ◐ = Moderate risk</p>
    
    <table>
        <thead>
            <tr>
                <th>Rank</th>
                <th>Player</th>
                <th>Hand</th>
                <th>Age</th>
                <th>Lvl</th>
                <th>IP</th>
                <th>FIP</th>
                <th>K/9</th>
                <th>BB/9</th>
                <th>GB%</th>
                <th>Grade</th>
                <th>Role</th>
            </tr>
        </thead>
        <tbody>')

for(i in 1:nrow(top10_safe)) {
  row <- top10_safe[i,]
  grade_class <- ifelse(row$overall_grade == "A", "grade-a", 
                        ifelse(row$overall_grade == "B+", "grade-b-plus", ""))
  
  html <- paste0(html, sprintf('
            <tr>
                <td class="num">%d</td>
                <td>%s%s</td>
                <td>%s</td>
                <td class="num">%d</td>
                <td>%s</td>
                <td class="num">%.0f</td>
                <td class="num">%.2f</td>
                <td class="num">%.1f</td>
                <td class="num">%.1f</td>
                <td class="num">%s</td>
                <td class="%s"><strong>%s</strong></td>
                <td>%s</td>
            </tr>',
    row$overall_rank,
    ifelse(row$injury_flag != "", paste0('<span class="badge-injury">', row$injury_flag, '</span> '), ''),
    row$player_name,
    ifelse(is.na(row$pitcher_hand), "R", row$pitcher_hand),
    row$latest_age,
    row$latest_level,
    row$total_ip,
    row$career_fip,
    row$career_k9,
    row$career_bb9,
    ifelse(is.na(row$career_gb_pct), "—", sprintf("%.0f", row$career_gb_pct)),
    grade_class,
    row$overall_grade,
    row$mlb_role
  ))
}

html <- paste0(html, '
        </tbody>
    </table>
    
    <div class="two-col">
        <div>
            <h2>Top 5 LHP Specialists</h2>
            <table>
                <thead>
                    <tr>
                        <th>Rank</th>
                        <th>Player</th>
                        <th>Age</th>
                        <th>FIP</th>
                        <th>K/9</th>
                        <th>Grade</th>
                    </tr>
                </thead>
                <tbody>')

for(i in 1:nrow(top_lhp)) {
  row <- top_lhp[i,]
  grade_class <- ifelse(row$overall_grade == "A", "grade-a", 
                        ifelse(row$overall_grade == "B+", "grade-b-plus", ""))
  
  html <- paste0(html, sprintf('
                    <tr>
                        <td class="num">%d</td>
                        <td>%s%s</td>
                        <td class="num">%d</td>
                        <td class="num">%.2f</td>
                        <td class="num">%.1f</td>
                        <td class="%s"><strong>%s</strong></td>
                    </tr>',
    row$overall_rank,
    ifelse(row$injury_flag != "", paste0('<span class="badge-injury">', row$injury_flag, '</span> '), ''),
    row$player_name,
    row$latest_age,
    row$career_fip,
    row$career_k9,
    grade_class,
    row$overall_grade
  ))
}

html <- paste0(html, '
                </tbody>
            </table>
        </div>
        
        <div>
            <h2>Immediate Actions</h2>
            <ul>
                <li><strong>Medical Review:</strong> Verify Monegro, Tebrake, Smeltz, Paez injury status</li>
                <li><strong>LHP Priority:</strong> Board 3+ LHP if team needs lefty depth</li>
                <li><strong>MLB-Ready:</strong> Reifert, Pushard, Birdsell (AAA, age 25+)</li>
                <li><strong>High Upside:</strong> Altermatt, Coleman (A grade, age ≤24)</li>
            </ul>
            
            <h2>Methodology</h2>
            <ul>
                <li><strong>Composite Score:</strong> Performance (50%) + Context (30%) + Projection (20%)</li>
                <li><strong>Performance:</strong> FIP, K/9, BB/9, K-BB</li>
                <li><strong>Context:</strong> Age/Level, GB%, HR/9, consistency</li>
                <li><strong>Grade Scale:</strong> A (75+), B+ (65-74), B (55-64)</li>
            </ul>
        </div>
    </div>
    
    <div class="footer">
        <strong>Analysis:</strong> December 2025 | <strong>Data:</strong> FanGraphs MiLB (2021-2025), 527 pitchers, 1,774 season records | 
        <strong>Injury Research:</strong> Public sources | <strong>Contact:</strong> [Your Name/Email] | 
        <strong>Full Report:</strong> Rule5_Enhanced_Report.html
    </div>
    
    <p class="no-print" style="margin-top: 20px; padding: 15px; background: #e8f5e9; border: 2px solid #4caf50;">
        <strong>Print Instructions:</strong> File → Print → Save as PDF or print directly. Optimized for single-page letter format.
    </p>
</body>
</html>')

# Write file
writeLines(html, "reports/Winter_Meetings_OnePager.html")

cat("\n✅ WINTER MEETINGS ONE-PAGER GENERATED!\n")
cat("   📄 File: reports/Winter_Meetings_OnePager.html\n")
cat("   ✓ Print-ready single page\n")
cat("   ✓ Top 10 targets (injury-adjusted)\n")
cat("   ✓ Top 5 LHP specialists\n")
cat("   ✓ Action items + methodology\n")
cat("   ✓ Data-dense, no fluff\n\n")
cat("💡 Open in browser and print to PDF for distribution\n\n")
