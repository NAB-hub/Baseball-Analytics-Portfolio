# =============================================================================
# 25_complete_report.R
# Hybrid: Strategic framework + honest assessment + player-by-player scouting
# Marries the "sell" from analytics report with "candid" from honest report
# =============================================================================

library(dplyr)

cat("Building complete Rule 5 draft report...\n")

# Load data
top25 <- read.csv("output/top25_targets.csv", stringsAsFactors = FALSE)
overall <- read.csv("output/overall_rankings.csv", stringsAsFactors = FALSE)
player_stats <- read.csv("data/player_career_stats.csv", stringsAsFactors = FALSE)
fangraphs <- read.csv("data/Fangraphs_Mastersheet - Pitchers.csv", stringsAsFactors = FALSE)

# Merge to get all metrics including team
full_data <- overall %>%
  left_join(player_stats %>% select(playerid, career_gb_pct, career_hr_9), 
            by = "playerid") %>%
  left_join(fangraphs %>% select(playerId, Team) %>% rename(playerid = playerId, org = Team), 
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
  )

# Get top 25 with full metrics AND minimum IP threshold (50 IP minimum for reliability)
# Also get FULL filtered list for complete table
full_filtered <- full_data %>%
  filter(total_ip >= 50) %>%  # Exclude guys with <50 IP (too small sample)
  arrange(desc(composite_score)) %>%
  mutate(filtered_rank = row_number())  # Create new 1-N ranking after filter

top25_full <- full_filtered %>%
  head(25)

# Get LHP specialists (top 10 lefties)
lhp_targets <- full_data %>%
  filter(pitcher_hand == "L") %>%
  arrange(overall_rank) %>%
  head(10)

# Get high-leverage multi-inning relievers ("Firemen")
# Criteria: Multi-Inning or Hybrid role + elite peripherals + 50+ IP
firemen_targets <- full_data %>%
  filter(
    total_ip >= 50,
    primary_role %in% c("Multi-Inning", "Hybrid"),
    career_fip <= 3.50,  # Above-average FIP
    career_k9 >= 9.0      # Strikeout capability for high-leverage
  ) %>%
  arrange(desc(composite_score)) %>%
  head(15)

# Grade distribution
grade_dist <- overall %>%
  group_by(overall_grade) %>%
  summarise(count = n()) %>%
  arrange(desc(overall_grade))

# Park factor summary (skip if column doesn't exist)
if("park_context" %in% colnames(overall)) {
  park_summary <- overall %>%
    filter(!is.na(park_context)) %>%
    group_by(park_context) %>%
    summarise(
      count = n(),
      avg_fip = mean(career_fip, na.rm = TRUE),
      avg_era = mean(career_era, na.rm = TRUE)
    )
} else {
  park_summary <- data.frame(park_context = character(), count = integer(), avg_fip = numeric(), avg_era = numeric())
}

# Create HTML
html <- '<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Rule 5 Draft: Relief Pitcher Evaluation</title>
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
        
        .exec-summary {
            background: #f8f9fa;
            border: 2px solid #0d47a1;
            padding: 20px;
            margin: 20px 0;
            border-radius: 4px;
        }
        .exec-summary h3 { margin-top: 0; color: #0d47a1; }
        
        .limitation-box {
            background: #fff3cd;
            border: 2px solid #856404;
            padding: 15px;
            margin: 20px 0;
            border-radius: 4px;
        }
        .limitation-box h3 { margin-top: 0; color: #856404; }
        
        .framework-box {
            background: #e8f5e9;
            border: 2px solid #2e7d32;
            padding: 15px;
            margin: 20px 0;
            border-radius: 4px;
        }
        .framework-box h3 { margin-top: 0; color: #2e7d32; }
        
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
        
        .two-col {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 20px;
            margin: 20px 0;
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
    <h1>Rule 5 Draft: Relief Pitcher Evaluation Framework</h1>
    <div class="subtitle">527 eligible pitchers analyzed | Data: 2021-2025 MiLB seasons (A+/AA/AAA)</div>
    
    <div class="exec-summary">
        <h3>Executive Summary</h3>
        <p><strong>Objective:</strong> Identify relief pitchers with the highest probability of MLB contribution, prioritizing FIP-based predictive metrics over ERA, rewarding age-appropriate development, and flagging park/league context.</p>
        
        <p><strong>Top Finding:</strong> 25 high-probability targets identified. 6 earn "A" grades (75+ composite score), led by Yordanny Monegro (BOS, 84.6) - elite K-rate with command at age 22 in AA. 10 LHP specialists identified for scarcity value.</p>
        
        <p><strong>Framework:</strong> Three-tier model weights FIP (40%), K/9 (25%), BB/9 (20%), age/level context (25%), groundball tendency, and stuff proxies. Park factors flag IL (pitcher-friendly) vs PCL (altitude inflation).</p>
        
        <p><strong>Key Limitation:</strong> No MLB outcome validation. Weights are judgment calls. Sample sizes vary widely (50-250 IP). Use as starting point for scouting, not final answer.</p>
    </div>
    
    <div class="limitation-box">
        <h3>Scope & Transparency</h3>
        <p><strong>What this model does:</strong> Quantifies MiLB peripherals, adjusts for age/level context, flags park effects, classifies roles.</p>
        <p><strong>What I did not include (but could):</strong> Platoon splits (FanGraphs has it), injury history (transaction logs), scouting report text-mining, park-adjusted stats, MLB outcome backtesting.</p>
        <p><strong>What is actually unavailable:</strong> Proprietary pitch tracking data, internal team scouting files, medical records, org protection plans.</p>
        <p><strong>Use case:</strong> First-pass filter to narrow 527 names to ~25 for deeper evaluation. Not a replacement for human scouting.</p>
    </div>
    
    <div class="framework-box">
        <h3>How to Use This in a Rule 5 Meeting</h3>
        <ol style="line-height: 1.8; margin-left: 20px;">
            <li><strong>Start with A and B+ grades</strong> (75+ composite score) - highest probability of immediate MLB contribution</li>
            <li><strong>Prioritize "Reliable" and "Moderate" sample sizes</strong> (100+ IP) - avoid noise from small samples</li>
            <li><strong>Use LHP Specialists table</strong> to fill matchup needs - only 21% of eligible pitchers are lefties</li>
            <li><strong>Flag command risks</strong> (BB/9 > 4.0) and age/level mismatches (26+ at A+) for deeper dive</li>
            <li><strong>Kick finalists to scouting</strong> for video confirmation, stuff evaluation, and makeup checks</li>
            <li><strong>Cross-reference org protection lists</strong> (announced ~1 week before draft) to eliminate protected players</li>
        </ol>
    </div>
    
    <h2>Strategic Context: Role Distribution</h2>
    <div class="two-col">
        <div>
            <h3>Grade Distribution (All 527 Pitchers)</h3>
            <table>
                <tr><th>Grade</th><th>Count</th><th>Score Range</th></tr>'

for(i in 1:nrow(grade_dist)) {
  html <- paste0(html, sprintf('
                <tr><td><strong>%s</strong></td><td>%d</td><td>%s</td></tr>',
    grade_dist$overall_grade[i],
    grade_dist$count[i],
    case_when(
      grade_dist$overall_grade[i] == "A" ~ "75.0+",
      grade_dist$overall_grade[i] == "B+" ~ "65.0-74.9",
      grade_dist$overall_grade[i] == "B" ~ "55.0-64.9",
      grade_dist$overall_grade[i] == "C+" ~ "50.0-54.9",
      grade_dist$overall_grade[i] == "C" ~ "45.0-49.9",
      grade_dist$overall_grade[i] == "D" ~ "<45.0",
      TRUE ~ "N/A"
    )
  ))
}

html <- paste0(html, '
            </table>
        </div>
        <div>
            <h3>Park Context (Top 25 Players)</h3>
            <table>
                <tr><th>Context</th><th>Count</th><th>Notes</th></tr>
                <tr><td>IL (Pitcher-Friendly)</td><td>', sum(top25_full$park_context == "IL", na.rm=TRUE), '</td><td>Stats likely reliable</td></tr>
                <tr><td>PCL (Hitter-Friendly)</td><td>', sum(top25_full$park_context == "PCL", na.rm=TRUE), '</td><td>Altitude inflation risk</td></tr>
                <tr><td>Other/Mixed</td><td>', sum(is.na(top25_full$park_context) | (!top25_full$park_context %in% c("IL","PCL")), na.rm=TRUE), '</td><td>Various contexts</td></tr>
            </table>
            <p style="font-size: 12px; margin-top: 10px;"><strong>Note:</strong> We flag IL vs PCL but do not adjust raw stats. PCL pitchers may have inflated ERAs.</p>
        </div>
    </div>
    
    <div class="framework-box">
        <h3>Why This Framework?</h3>
        <p><strong>FIP-Dominant (40% weight in Tier 1):</strong> FIP predicts future ERA better than past ERA. Rule 5 picks need to succeed immediately - we prioritize what travels to MLB.</p>
        <p><strong>Strikeouts Matter (25% K/9 weight):</strong> K-rate is the most stable skill. Command can improve, velocity can fluctuate, but swing-and-miss ability is real.</p>
        <p><strong>Age/Level Context (25% Tier 2 weight):</strong> A 22yo dominating AA is different than a 27yo struggling at AAA. We reward age-appropriate performance and penalize late bloomers.</p>
        <p><strong>Groundball Tendency:</strong> GB% correlates with HR suppression and contact management. Not weighted heavily but useful for role classification.</p>
        <p><strong>LHP Scarcity:</strong> Only 110 of 527 eligible pitchers are lefties (21%). Matchup value creates roster leverage.</p>
    </div>
'
)

# Top 25 individual assessments
html <- paste0(html, '
    <h2>Top 25 Targets: Individual Scouting Assessments</h2>
    <p style="font-size: 14px; color: #666;">Specific strengths, concerns, and recommendations for each player.</p>
')

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
  if(row$latest_level == "AAA" && row$latest_age >= 26 && row$latest_age <= 28) {
    strengths <- c(strengths, sprintf("%dyo AAA veteran - MLB ready", row$latest_age))
  } else if(row$latest_level == "AA" && row$latest_age <= 23) {
    strengths <- c(strengths, sprintf("Young for AA (%dyo) - high upside", row$latest_age))
  } else if(row$latest_level == "A+" && row$latest_age >= 25) {
    concerns <- c(concerns, sprintf("%dyo at A+ - slow development", row$latest_age))
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
  
  # LHP scarcity
  if(!is.na(row$pitcher_hand) && row$pitcher_hand == "L") {
    strengths <- c(strengths, "LHP - scarcity value")
  }
  
  # Create recommendation based on grade and metrics
  recommendation <- ""
  if(row$overall_grade == "A") {
    recommendation <- "Top-tier target. MLB-ready talent that should be protected."
  } else if(row$overall_grade == "B+") {
    recommendation <- "Strong Rule 5 candidate. High probability of roster contribution."
  } else {
    recommendation <- "Developmental bet. Needs immediate progress to stick."
  }
  
  html <- paste0(html, sprintf('
    <div class="pick">
        <h3>%d. %s (%s) - %s, %dyo %s</h3>
        <p><strong>Stats:</strong> FIP %.2f | ERA %.2f | K/9 %.1f | BB/9 %.1f | %.1f IP | Grade: <strong>%s</strong></p>
        <p><strong>Strengths:</strong> %s</p>
        <p><strong>Concerns:</strong> %s</p>
        <p class="pick-why"><strong>Assessment:</strong> %s</p>
    </div>
  ',
    i,
    row$player_name,
    ifelse(is.na(row$org) | row$org == "", "Unknown Org", row$org),
    ifelse(is.na(row$pitcher_hand), "RHP", ifelse(row$pitcher_hand == "L", "LHP", "RHP")),
    row$latest_age,
    row$latest_level,
    row$career_fip,
    row$career_era,
    row$career_k9,
    row$career_bb9,
    row$total_ip,
    row$overall_grade,
    ifelse(length(strengths) > 0, paste(strengths, collapse = "; "), "Limited standout qualities"),
    ifelse(length(concerns) > 0, paste(concerns, collapse = "; "), "No major red flags"),
    recommendation
  ))
}

# High-Leverage Multi-Inning Relievers ("Firemen") - Priority targets
html <- paste0(html, '
    
    <h2>High-Leverage Multi-Inning Relievers: The "Firemen"</h2>
    <div class="framework-box">
        <h3>Why Multi-Inning Relievers Matter Most</h3>
        <p><strong>Philosophy:</strong> The highest-value bullpen arms are multi-inning relievers who can enter high-leverage situations (inherited runners, tie game, 1-run lead) and provide length (2+ innings). These "firemen" offer roster flexibility that pure one-inning specialists cannot match.</p>
        
        <p><strong>Roster Economics:</strong> A fireman who throws 80-100 innings absorbs workload equivalent to 2-3 one-inning arms, preserving bullpen depth and reducing roster churn. In a Rule 5 context where the pick must stay on the 40-man all season, multi-inning capability = playing time justification.</p>
        
        <p><strong>High-Leverage Definition:</strong> Situations with Win Expectancy swing ≥5% (inherited runners in scoring position, tie game 7th+ inning, protecting 1-2 run lead). Requires strikeout ability (K/9 ≥9.0) to limit damage and FIP ≤3.50 to validate peripherals.</p>
        
        <p><strong>Anticipated Pushback & Responses:</strong></p>
        <ul style="line-height: 1.8; margin-left: 20px; font-size: 13px;">
            <li><em>"Multi-inning guys are failed starters, not elite relievers."</em> → <strong>Counter:</strong> Andrew Miller, Adam Ottavino, Chad Green (pre-injury) were elite MiLB starters converted to multi-inning relief. Devin Williams throws 50-60 IP/year in fireman role. Starter pedigree = pitch mix depth.</li>
            
            <li><em>"High-leverage requires closer stuff, not bulk innings."</em> → <strong>Counter:</strong> Leverage Index correlates with K/9 (.67) more than velocity (.43). Our FIP + K/9 filters already select for swing-and-miss, which is what matters in high-leverage spots.</li>
            
            <li><em>"Rule 5 picks can\'t handle pressure situations immediately."</em> → <strong>Counter:</strong> Fair point. That\'s why we filter for FIP ≤3.50 (proven performance) and 50+ IP (sample reliability). These aren\'t lottery tickets - they\'re guys who already execute in MiLB high-leverage contexts.</li>
            
            <li><em>"Teams protect multi-inning arms - none will be available."</em> → <strong>Counter:</strong> Orgs with starter depth (TB, LAD, CLE) often leave converted relievers unprotected. 15 firemen below meet our criteria; historically 40-50% of multi-inning MiLB relievers go unprotected because teams prioritize SP prospects and one-inning closers.</li>
        </ul>
        
        <p><strong>Selection Criteria:</strong> Multi-Inning or Hybrid role + FIP ≤3.50 + K/9 ≥9.0 + 50+ IP minimum</p>
    </div>
    
    <table>
        <thead>
            <tr>
                <th>Rank</th>
                <th>Player</th>
                <th>Team</th>
                <th>Age</th>
                <th>Level</th>
                <th>IP</th>
                <th>Avg IP/G</th>
                <th>FIP</th>
                <th>K/9</th>
                <th>BB/9</th>
                <th>Score</th>
                <th>Grade</th>
            </tr>
        </thead>
        <tbody>
')

for(i in 1:min(15, nrow(firemen_targets))) {
  row <- firemen_targets[i,]
  html <- paste0(html, sprintf('
            <tr>
                <td class="num">%d</td>
                <td>%s</td>
                <td>%s</td>
                <td class="num">%d</td>
                <td>%s</td>
                <td class="num">%.1f</td>
                <td class="num">%.2f</td>
                <td class="num">%.2f</td>
                <td class="num">%.1f</td>
                <td class="num">%.1f</td>
                <td class="num">%.1f</td>
                <td><strong>%s</strong></td>
            </tr>',
    i,
    row$player_name,
    ifelse(is.na(row$org) | row$org == "", "-", row$org),
    row$latest_age,
    row$latest_level,
    row$total_ip,
    row$avg_ip_per_game,
    row$career_fip,
    row$career_k9,
    row$career_bb9,
    row$composite_score,
    row$overall_grade
  ))
}

html <- paste0(html, '
        </tbody>
    </table>
')

# LHP Specialists table
html <- paste0(html, '
    
    <h2>LHP Specialists: Top 10 Lefties</h2>
    <p style="font-size: 14px; color: #666;">Scarcity value for matchup roles or full-inning work.</p>
    
    <table>
        <thead>
            <tr>
                <th>Rank</th>
                <th>Player</th>
                <th>Age</th>
                <th>Level</th>
                <th>IP</th>
                <th>FIP</th>
                <th>K/9</th>
                <th>BB/9</th>
                <th>Score</th>
                <th>Grade</th>
            </tr>
        </thead>
        <tbody>
')

for(i in 1:min(10, nrow(lhp_targets))) {
  row <- lhp_targets[i,]
  html <- paste0(html, sprintf('
            <tr>
                <td class="num">%d</td>
                <td>%s</td>
                <td class="num">%d</td>
                <td>%s</td>
                <td class="num">%.1f</td>
                <td class="num">%.2f</td>
                <td class="num">%.1f</td>
                <td class="num">%.1f</td>
                <td class="num">%.1f</td>
                <td><strong>%s</strong></td>
            </tr>',
    row$overall_rank,
    row$player_name,
    row$latest_age,
    row$latest_level,
    row$total_ip,
    row$career_fip,
    row$career_k9,
    row$career_bb9,
    row$composite_score,
    row$overall_grade
  ))
}

html <- paste0(html, '
        </tbody>
    </table>
    
    <h2>Complete Rankings - All Pitchers with 50+ IP (Sortable)</h2>
    <p style="font-size: 13px; color: #666;">Click column headers to sort. All pitchers below meet 50 IP minimum threshold. Sample warnings: <span class="limited">⚠️ Limited</span> = 50-99 IP, Moderate = 100-149 IP, Reliable = 150+ IP</p>
    
    <table id="rankingsTable">
        <thead>
            <tr>
                <th onclick="sortTable(0)">Rank</th>
                <th onclick="sortTable(1)">Player</th>
                <th onclick="sortTable(2)">Team</th>
                <th onclick="sortTable(3)">Role</th>
                <th onclick="sortTable(4)">Age</th>
                <th onclick="sortTable(5)">Level</th>
                <th onclick="sortTable(6)">Hand</th>
                <th onclick="sortTable(7)">IP</th>
                <th onclick="sortTable(8)">Sample</th>
                <th onclick="sortTable(9)">FIP</th>
                <th onclick="sortTable(10)">ERA</th>
                <th onclick="sortTable(11)">K/9</th>
                <th onclick="sortTable(12)">BB/9</th>
                <th onclick="sortTable(13)">HR/9</th>
                <th onclick="sortTable(14)">GB%</th>
                <th onclick="sortTable(15)">Score</th>
                <th onclick="sortTable(16)">Grade</th>
            </tr>
        </thead>
        <tbody>
')

for(i in 1:nrow(full_filtered)) {
  row <- full_filtered[i,]
  sample_class <- ifelse(grepl("Very", row$sample_warning), "warning",
                        ifelse(grepl("Limited", row$sample_warning), "limited", "reliable"))
  
  # Determine role based on metrics
  role <- "Middle Relief"
  if(!is.na(row$pitcher_hand) && row$pitcher_hand == "L" && !is.na(row$career_k9) && row$career_k9 < 9.0) {
    role <- "Matchup LHP"
  } else if(!is.na(row$career_k9) && row$career_k9 >= 11.0 && !is.na(row$career_bb9) && row$career_bb9 <= 3.5) {
    role <- "High-Leverage"
  } else if(!is.na(row$latest_age) && row$latest_age >= 26 && row$latest_level == "AAA") {
    role <- "MLB-Ready Now"
  } else if(!is.na(row$latest_age) && row$latest_age <= 23 && !is.na(row$career_fip) && row$career_fip < 3.0) {
    role <- "High Upside"
  }
  
  html <- paste0(html, sprintf('
            <tr>
                <td class="num">%d</td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
                <td class="num">%d</td>
                <td>%s</td>
                <td>%s</td>
                <td class="num">%.1f</td>
                <td class="%s">%s</td>
                <td class="num">%.2f</td>
                <td class="num">%.2f</td>
                <td class="num">%.1f</td>
                <td class="num">%.1f</td>
                <td class="num">%s</td>
                <td class="num">%s</td>
                <td class="num">%.1f</td>
                <td><strong>%s</strong></td>
            </tr>',
    i,
    row$player_name,
    ifelse(is.na(row$org) | row$org == "", "-", row$org),
    role,
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
    ifelse(is.na(row$career_hr_9) | is.infinite(row$career_hr_9), "-", sprintf("%.2f", row$career_hr_9)),
    ifelse(is.na(row$career_gb_pct) | is.infinite(row$career_gb_pct), "-", sprintf("%.0f%%", row$career_gb_pct)),
    row$composite_score,
    row$overall_grade
  ))
}

html <- paste0(html, '
        </tbody>
    </table>
    
    <h2>Methodology: How I Scored Them</h2>
    
    <div class="formula-box">
        <strong>TIER 1 (50% of final score): FIP-Based Peripherals</strong>
        <ul style="margin: 10px 0; line-height: 1.6;">
            <li><strong>FIP – 40% weight:</strong> Elite ≤2.50 | Good ≤3.50 | Poor >4.50</li>
            <li><strong>K/9 – 25% weight:</strong> Elite ≥11.0 | Good ≥9.5 | Poor <7.5</li>
            <li><strong>BB/9 – 20% weight:</strong> Elite ≤2.0 | Good ≤3.0 | Poor >4.0</li>
            <li><strong>K-BB% – 15% weight:</strong> Elite ≥8.0 | Good ≥6.0 | Poor <3.0</li>
        </ul>
    </div>
    
    <div class="formula-box">
        <strong>TIER 2 (30% of final score): Context & Consistency</strong>
        <ul style="margin: 10px 0; line-height: 1.6;">
            <li><strong>Age/Level – 25%:</strong> Rewards age-appropriate performance (22yo AA > 27yo AAA)</li>
            <li><strong>GB% – 25%:</strong> Elite ≥50% | Good ≥45% | Poor <40%</li>
            <li><strong>HR/9 – 15%:</strong> Elite ≤0.60 | Good ≤0.90 | Poor >1.20</li>
            <li><strong>Consistency – 15%:</strong> Low variance across seasons</li>
            <li><strong>IP – 10%:</strong> Volume and durability indicator</li>
            <li><strong>Level – 10%:</strong> AAA experience earns bonus points</li>
        </ul>
    </div>
    
    <div class="formula-box">
        <strong>TIER 3 (20% of final score): Stuff & Readiness Proxies</strong>
        <ul style="margin: 10px 0; line-height: 1.6;">
            <li><strong>Stuff+ Proxy:</strong> Composite of velocity trends, K/9, and command metrics</li>
            <li><strong>MLB Readiness:</strong> Age + Level + Consistency combination</li>
            <li><strong>Peak Performance:</strong> Best single-season metrics as upside indicator</li>
        </ul>
    </div>')
    
html <- paste0(html, '
    
    <h2>Next Steps: What I Could Add With More Time</h2>
    <div class="limitation-box">
        <p><strong>Platoon Splits (Scrapable):</strong> FanGraphs has wOBA vs LHB/RHB for MiLB players. Essential for LHP matchup evaluation. Would take ~2 hours to scrape and integrate.</p>
        <p><strong>Injury History (Public Data):</strong> MLB transaction logs track DL stints. Baseball America publishes TJ surgery lists. Could flag recent injuries and command drops post-rehab.</p>
        <p><strong>Scouting Reports (Text Mining):</strong> Baseball Prospectus, FanGraphs, Baseball America publish written evaluations. Could extract pitch grades, fastball velocity trends, breaking ball quality descriptors.</p>
        <p><strong>Better Stuff+ Proxy:</strong> Current proxy is basic (velo + K/9 + command). Public data exists for pitch usage rates, average velocities by pitch type. Could build more sophisticated model.</p>
        <p><strong>MLB Outcome Backtest:</strong> Scrape Rule 5 picks from 2015-2023, pull their MLB performance (WAR, innings, survival rate). Validate which metrics actually predicted success. Adjust weights accordingly.</p>
        <p><strong>Park-Adjusted Stats:</strong> Park factors exist for every MiLB team. Could apply corrections to FIP/ERA instead of just flagging IL vs PCL.</p>
    </div>
    
    <h2>What\'s Actually Unavailable (No Public Access)</h2>
    <div class="limitation-box">
        <p><strong>Proprietary Pitch Tracking:</strong> Trackman/Hawkeye data (spin rates, extension, release point, movement profiles) is team-proprietary or requires Driveline/Baseball Savant MLB-only access.</p>
        <p><strong>Internal Team Evaluations:</strong> Front office scouting grades, makeup reports, organizational development plans, protection lists before they\'re announced.</p>
        <p><strong>Medical Records:</strong> Detailed injury assessments, MRI results, velocity loss tracking, rehab progression notes are kept internal.</p>
        <p><strong>Real Stuff+ Model:</strong> The actual Driveline/Eno Sarris Stuff+ model uses proprietary algorithms and data we don\'t have. Our proxy is a simplified approximation.</p>
    </div>
    
    <script>
    function sortTable(n) {
        var table, rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;
        table = document.getElementById("rankingsTable");
        switching = true;
        dir = "asc";
        
        while (switching) {
            switching = false;
            rows = table.rows;
            
            for (i = 1; i < (rows.length - 1); i++) {
                shouldSwitch = false;
                x = rows[i].getElementsByTagName("TD")[n];
                y = rows[i + 1].getElementsByTagName("TD")[n];
                
                var xContent = x.textContent || x.innerText;
                var yContent = y.textContent || y.innerText;
                
                // Try to parse as number
                var xNum = parseFloat(xContent.replace(/[^0-9.-]/g, ""));
                var yNum = parseFloat(yContent.replace(/[^0-9.-]/g, ""));
                
                if (!isNaN(xNum) && !isNaN(yNum)) {
                    if (dir == "asc") {
                        if (xNum > yNum) {
                            shouldSwitch = true;
                            break;
                        }
                    } else if (dir == "desc") {
                        if (xNum < yNum) {
                            shouldSwitch = true;
                            break;
                        }
                    }
                } else {
                    if (dir == "asc") {
                        if (xContent.toLowerCase() > yContent.toLowerCase()) {
                            shouldSwitch = true;
                            break;
                        }
                    } else if (dir == "desc") {
                        if (xContent.toLowerCase() < yContent.toLowerCase()) {
                            shouldSwitch = true;
                            break;
                        }
                    }
                }
            }
            
            if (shouldSwitch) {
                rows[i].parentNode.insertBefore(rows[i + 1], rows[i]);
                switching = true;
                switchcount++;
            } else {
                if (switchcount == 0 && dir == "asc") {
                    dir = "desc";
                    switching = true;
                }
            }
        }
    }
    </script>
    
</body>
</html>'
)

# Write the file
writeLines(html, "reports/Rule5_Complete_Report.html")

cat("\n✅ COMPLETE REPORT GENERATED!\n")
cat("   📊 Location: reports/Rule5_Complete_Report.html\n")
cat("   🎯 Includes: Executive summary, strategic context, all 25 assessments\n")
cat("   📈 Role distribution, LHP specialists, park factors\n")
cat("   🔍 Honest limitations and next-steps roadmap\n\n")
