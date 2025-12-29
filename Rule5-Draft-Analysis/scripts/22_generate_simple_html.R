# =============================================================================
# 22_generate_simple_html.R
# Generate HTML report without pandoc dependency
# =============================================================================

library(dplyr)

cat("📄 Generating HTML Report (no pandoc required)...\n\n")

# Load data
top25 <- read.csv("output/top25_targets.csv", stringsAsFactors = FALSE)
overall <- read.csv("output/overall_rankings.csv", stringsAsFactors = FALSE)
lhp <- read.csv("output/top10_lhp_specialists.csv", stringsAsFactors = FALSE)

# Get project root
project_root <- getwd()

# Create HTML content
html_content <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Rule 5 Draft Relief Pitcher Analytics</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            color: #212121;
            background: #fafafa;
            padding: 20px;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            padding: 40px;
            border-radius: 8px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        
        h1 {
            color: #0d47a1;
            font-size: 2.5em;
            margin-bottom: 10px;
            border-bottom: 4px solid #0d47a1;
            padding-bottom: 10px;
        }
        
        h2 {
            color: #0d47a1;
            font-size: 1.8em;
            margin-top: 40px;
            margin-bottom: 15px;
            border-left: 5px solid #f57c00;
            padding-left: 15px;
        }
        
        h3 {
            color: #1976d2;
            font-size: 1.3em;
            margin-top: 25px;
            margin-bottom: 10px;
        }
        
        .subtitle {
            color: #757575;
            font-size: 1.2em;
            margin-bottom: 30px;
        }
        
        .date {
            color: #9e9e9e;
            font-size: 0.9em;
            margin-bottom: 30px;
        }
        
        .executive-summary {
            background: #e3f2fd;
            padding: 25px;
            border-radius: 8px;
            border-left: 5px solid #0d47a1;
            margin-bottom: 30px;
        }
        
        .key-finding {
            background: #fff3e0;
            padding: 20px;
            border-radius: 8px;
            border-left: 5px solid #f57c00;
            margin: 15px 0;
        }
        
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
            font-size: 0.9em;
        }
        
        th {
            background: #0d47a1;
            color: white;
            padding: 12px 8px;
            text-align: left;
            font-weight: 600;
        }
        
        td {
            padding: 10px 8px;
            border-bottom: 1px solid #e0e0e0;
        }
        
        tr:nth-child(even) {
            background: #f5f5f5;
        }
        
        tr:hover {
            background: #e3f2fd;
        }
        
        .grade-A { background: #c8e6c9; font-weight: bold; }
        .grade-B { background: #fff9c4; }
        .grade-C { background: #ffccbc; }
        
        .visualization {
            margin: 30px 0;
            text-align: center;
        }
        
        .visualization img {
            max-width: 100%;
            height: auto;
            border-radius: 8px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        
        .viz-caption {
            font-style: italic;
            color: #757575;
            margin-top: 10px;
            font-size: 0.9em;
        }
        
        .metric-box {
            display: inline-block;
            background: white;
            border: 2px solid #0d47a1;
            border-radius: 8px;
            padding: 15px 25px;
            margin: 10px;
            text-align: center;
        }
        
        .metric-value {
            font-size: 2em;
            font-weight: bold;
            color: #0d47a1;
        }
        
        .metric-label {
            font-size: 0.9em;
            color: #757575;
            margin-top: 5px;
        }
        
        .park-factor-il {
            background: #c8e6c9;
            padding: 5px 10px;
            border-radius: 4px;
            font-weight: 600;
        }
        
        .park-factor-pcl {
            background: #ffccbc;
            padding: 5px 10px;
            border-radius: 4px;
            font-weight: 600;
        }
        
        ul, ol {
            margin-left: 20px;
            margin-bottom: 15px;
        }
        
        li {
            margin-bottom: 8px;
        }
        
        strong {
            color: #0d47a1;
        }
        
        .footer {
            margin-top: 50px;
            padding-top: 20px;
            border-top: 2px solid #e0e0e0;
            text-align: center;
            color: #757575;
            font-size: 0.9em;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>Rule 5 Draft Relief Pitcher Analytics</h1>
        <div class="subtitle">Top 25 Targets and Strategic Recommendations</div>
        <div class="date">December 1, 2025</div>
        
        <div class="executive-summary">
            <h2>Executive Summary</h2>
            <p>This report presents a <strong>comprehensive analytical framework</strong> for evaluating <strong>527 Rule 5 Draft-eligible relief pitchers</strong> using advanced metrics, predictive analytics, and context-aware scoring methodology.</p>
        </div>
        
        <h2>Key Analytics Framework</h2>
        
        <div class="key-finding">
            <h3>🎯 FIP-Dominant Approach (Predictive Over Results-Based)</h3>
            <ul>
                <li><strong>40% weight on FIP</strong> vs. traditional ERA-heavy models</li>
                <li><strong>Emphasis on K/9 (25%), BB/9 (20%), K-BB spread (15%)</strong></li>
                <li>Prioritizes skills pitchers control (strikeouts, walks) over defense-dependent outcomes</li>
                <li><strong>Result</strong>: Top 25 now features elite peripherals (FIP &lt;3.0) with MLB-ready command</li>
            </ul>
        </div>
        
        <div class="key-finding">
            <h3>📊 Context-Aware Age Scoring</h3>
            <ul>
                <li><strong>AAA Veterans rewarded</strong> (ages 26-28 = prime Rule 5 targets with experience)</li>
                <li><strong>AA Prospects</strong> evaluated on traditional development curve</li>
                <li><strong>A+ Youth premium</strong> for high-upside players</li>
                <li>Eliminates penalty for MLB-ready 27-year-olds at AAA (previously penalized as "too old")</li>
            </ul>
        </div>
        
        <div class="key-finding">
            <h3>🌐 Park Factor Intelligence</h3>
            <ul>
                <li><strong class="park-factor-il">International League (IL)</strong> - Pitcher-friendly, reliable stats ✅</li>
                <li><strong class="park-factor-pcl">Pacific Coast League (PCL)</strong> - Hitter-friendly with altitude inflation ⚠️</li>
                <li>Helps evaluate AAA pitchers in proper environmental context</li>
                <li>31 IL pitchers vs. 7 PCL pitchers analyzed for park-adjusted interpretation</li>
            </ul>
        </div>
        
        <h2>Population & Results</h2>
        <div style="text-align: center; margin: 30px 0;">
            <div class="metric-box">
                <div class="metric-value">527</div>
                <div class="metric-label">Eligible Pitchers</div>
            </div>
            <div class="metric-box">
                <div class="metric-value">25</div>
                <div class="metric-label">Top Targets</div>
            </div>
            <div class="metric-box">
                <div class="metric-value">87</div>
                <div class="metric-label">MLB-Ready</div>
            </div>
            <div class="metric-box">
                <div class="metric-value">110</div>
                <div class="metric-label">LHP Options</div>
            </div>
        </div>
        
        <h2>Top 25 Rule 5 Draft Targets</h2>
        
        <div class="visualization">
            <img src="plots/01_top25_rankings.png" alt="Top 25 Rankings">
            <div class="viz-caption">Figure 1: Composite scores for Top 25 targets (FIP-dominant framework)</div>
        </div>
        
        <table>
            <thead>
                <tr>
                    <th>Rank</th>
                    <th>Player</th>
                    <th>Hand</th>
                    <th>Type</th>
                    <th>Score</th>
                    <th>Grade</th>
                    <th>Age</th>
                    <th>Level</th>
                    <th>FIP</th>
                    <th>K/9</th>
                    <th>BB/9</th>
                </tr>
            </thead>
            <tbody>
')

# Add top 25 rows
for (i in 1:nrow(top25)) {
  row <- top25[i,]
  grade_class <- ifelse(grepl("A", row$overall_grade), "grade-A",
                       ifelse(grepl("B", row$overall_grade), "grade-B", "grade-C"))
  
  html_content <- paste0(html_content,
    sprintf('
                <tr class="%s">
                    <td><strong>%d</strong></td>
                    <td>%s</td>
                    <td>%s</td>
                    <td>%s</td>
                    <td>%.1f</td>
                    <td><strong>%s</strong></td>
                    <td>%d</td>
                    <td>%s</td>
                    <td>%.2f</td>
                    <td>%.1f</td>
                    <td>%.1f</td>
                </tr>
    ', grade_class, row$overall_rank, row$player_name, 
       ifelse(is.na(row$pitcher_hand), "R", row$pitcher_hand),
       row$pitcher_type, row$composite_score, row$overall_grade,
       row$latest_age, row$latest_level, row$career_fip, 
       row$career_k9, row$career_bb9))
}

html_content <- paste0(html_content, '
            </tbody>
        </table>
        
        <h2>Performance Analysis</h2>
        
        <div class="visualization">
            <img src="plots/02_tier_scatter.png" alt="Performance vs Projection">
            <div class="viz-caption">Figure 2: Current performance vs. future projection matrix</div>
        </div>
        
        <div class="visualization">
            <img src="plots/06_top15_heatmap.png" alt="Score Breakdown">
            <div class="viz-caption">Figure 3: Top 15 targets score breakdown across all tiers</div>
        </div>
        
        <h2>Pitcher Classification</h2>
        
        <div class="visualization">
            <img src="plots/03_pitcher_type_distribution.png" alt="Type Distribution">
            <div class="viz-caption">Figure 4: Distribution by pitcher role (527 total)</div>
        </div>
        
        <h2>Left-Handed Specialists</h2>
        
        <div class="visualization">
            <img src="plots/04_lhp_specialists.png" alt="LHP Specialists">
            <div class="viz-caption">Figure 5: Top 10 LHP options for matchup situations</div>
        </div>
        
        <table>
            <thead>
                <tr>
                    <th>LHP Rank</th>
                    <th>Player</th>
                    <th>Type</th>
                    <th>Score</th>
                    <th>Overall Rank</th>
                    <th>FIP</th>
                    <th>K/9</th>
                </tr>
            </thead>
            <tbody>
')

# Add LHP rows
for (i in 1:min(10, nrow(lhp))) {
  row <- lhp[i,]
  html_content <- paste0(html_content,
    sprintf('
                <tr>
                    <td><strong>%d</strong></td>
                    <td>%s</td>
                    <td>%s</td>
                    <td>%.1f</td>
                    <td>#%d</td>
                    <td>%.2f</td>
                    <td>%.1f</td>
                </tr>
    ', row$lhp_rank, row$player_name, row$primary_role,
       row$composite_score, row$overall_rank, row$career_fip, row$career_k9))
}

html_content <- paste0(html_content, '
            </tbody>
        </table>
        
        <h2>Park Factor Context</h2>
        
        <h3>International League vs. Pacific Coast League</h3>
        <p>AAA pitchers face dramatically different environmental conditions:</p>
        
        <table>
            <thead>
                <tr>
                    <th>League</th>
                    <th>Pitchers</th>
                    <th>Context</th>
                    <th>Avg FIP</th>
                    <th>Avg ERA</th>
                    <th>Reliability</th>
                </tr>
            </thead>
            <tbody>
')

# Add park factor data
aaa_il <- overall %>% filter(latest_level == "AAA", latest_aaa_league == "IL")
aaa_pcl <- overall %>% filter(latest_level == "AAA", latest_aaa_league == "PCL")

if (nrow(aaa_il) > 0) {
  html_content <- paste0(html_content,
    sprintf('
                <tr class="park-factor-il">
                    <td><strong>International League (IL)</strong></td>
                    <td>%d</td>
                    <td>Pitcher-friendly</td>
                    <td>%.2f</td>
                    <td>%.2f</td>
                    <td>✅ Trustworthy</td>
                </tr>
    ', nrow(aaa_il), mean(aaa_il$career_fip, na.rm=TRUE), 
       mean(aaa_il$career_era, na.rm=TRUE)))
}

if (nrow(aaa_pcl) > 0) {
  html_content <- paste0(html_content,
    sprintf('
                <tr class="park-factor-pcl">
                    <td><strong>Pacific Coast League (PCL)</strong></td>
                    <td>%d</td>
                    <td>Hitter-friendly (altitude)</td>
                    <td>%.2f</td>
                    <td>%.2f</td>
                    <td>⚠️ Inflated stats</td>
                </tr>
    ', nrow(aaa_pcl), mean(aaa_pcl$career_fip, na.rm=TRUE), 
       mean(aaa_pcl$career_era, na.rm=TRUE)))
}

html_content <- paste0(html_content, '
            </tbody>
        </table>
        
        <h2>Grade Distribution</h2>
        
        <div class="visualization">
            <img src="plots/05_grade_distribution.png" alt="Grade Distribution">
            <div class="viz-caption">Figure 6: Overall grade distribution (all 527 pitchers)</div>
        </div>
        
        <h2>Top 10 Metrics Comparison</h2>
        
        <div class="visualization">
            <img src="plots/07_top10_metrics.png" alt="Metrics Comparison">
            <div class="viz-caption">Figure 7: Key performance metrics for Top 10 targets</div>
        </div>
        
        <h2>Methodology Overview</h2>
        
        <h3>3-Tier Weighted System</h3>
        <ul>
            <li><strong>Tier 1 (50%)</strong>: FIP-Dominant Performance - FIP 40%, K/9 25%, BB/9 20%, K-BB 15%</li>
            <li><strong>Tier 2 (30%)</strong>: Context-Aware Analysis - Age/Level, GB%, HR/9, Consistency, Workload</li>
            <li><strong>Tier 3 (20%)</strong>: Future Projection - Stuff+ proxy, MLB readiness, Peak performance</li>
        </ul>
        
        <h3>Advanced Metrics</h3>
        <ul>
            <li><strong>Stuff+ Proxy</strong>: Velocity + K-rate + control + GB% + contact (95-109 range)</li>
            <li><strong>MLB Roster Projection</strong>: 5-factor model for stick probability</li>
            <li><strong>Level Weighting</strong>: AAA 3.0x, AA 2.0x, A+ 0.3x</li>
            <li><strong>Handedness</strong>: 382 LHP, 1,392 RHP properly classified</li>
        </ul>
        
        <h2>Data Sources</h2>
        <ul>
            <li><strong>Source</strong>: FanGraphs MiLB leaderboards and player pages</li>
            <li><strong>Time Period</strong>: 2021-2025 MiLB seasons</li>
            <li><strong>Population</strong>: 527 eligible pitchers, 1,774 season records</li>
            <li><strong>Levels</strong>: A+, AA, AAA (minimum 10 IP per season)</li>
        </ul>
        
        <div class="footer">
            <p><strong>Rule 5 Draft Relief Pitcher Analytics</strong></p>
            <p>Generated: ', format(Sys.time(), "%B %d, %Y at %I:%M %p"), '</p>
            <p>Analysis Framework: FIP-Dominant | Context-Aware | Park Factor Adjusted</p>
        </div>
    </div>
</body>
</html>
')

# Write HTML file
writeLines(html_content, "reports/Rule5_Draft_Report.html")

cat("\n✅ HTML REPORT GENERATED SUCCESSFULLY!\n")
cat("   🌐 Location: reports/Rule5_Draft_Report.html\n")
cat("   📊 Open in any web browser\n")
cat("   📱 Responsive design works on all devices\n\n")

cat("To view: Open reports/Rule5_Draft_Report.html in your web browser\n")
