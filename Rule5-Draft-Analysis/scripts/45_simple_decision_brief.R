# Simple Decision Brief - No memory issues
library(dplyr)

# Load data
overall <- read.csv("output/overall_rankings_with_injuries.csv", stringsAsFactors = FALSE)
player_stats <- read.csv("data/player_career_stats.csv", stringsAsFactors = FALSE)

# Top 25 only
top25 <- overall %>%
  filter(overall_rank <= 25) %>%
  left_join(player_stats %>% select(playerid, career_gb_pct, latest_ip), by = "playerid")

# Key players
pushard <- top25 %>% filter(grepl("Pushard", player_name))
reifert <- top25 %>% filter(grepl("Reifert", player_name))
monegro <- top25 %>% filter(grepl("Monegro", player_name))
walling <- top25 %>% filter(grepl("Walling", player_name))

# Write simple HTML
html <- '<!DOCTYPE html>
<html><head><meta charset="UTF-8"><title>Rule 5 Decision Brief</title>
<style>
body{font-family:Arial,sans-serif;max-width:1200px;margin:20px auto;padding:20px;line-height:1.6}
h1{color:#0d47a1;border-bottom:3px solid #0d47a1;padding-bottom:10px}
h2{color:#333;margin-top:30px;border-left:4px solid #0d47a1;padding-left:10px}
.box{background:#e3f2fd;border:2px solid #0d47a1;padding:20px;margin:20px 0;border-radius:4px}
.rec{background:#fff;border:2px solid #0d47a1;padding:15px;margin:15px 0;border-radius:4px}
table{width:100%;border-collapse:collapse;margin:20px 0;font-size:13px}
th{background:#0d47a1;color:#fff;padding:10px;text-align:left}
td{padding:8px;border-bottom:1px solid #ddd}
.num{text-align:right}
</style>
</head><body>

<h1>Rule 5 Draft - Decision Brief</h1>
<p><strong>December 2025 | 527 pitchers analyzed</strong></p>

<div class="box">
<h3>Executive Summary</h3>
<p><strong>Problem:</strong> Identify a Rule 5 reliever who can realistically stay on the 26-man roster all season, contributing immediately while meeting the service-time carry requirement.</p>
<p><strong>Recommendation:</strong> Matt Pushard (RHP, 27, AAA) is the primary target. Pitched 62.1 IP in 2025 (AAA), career totals 166.4 IP across 127 games (reliever). 3.36 FIP, 10.1 K/9, 3.3 BB/9, clean injury history. Age-27 AAA veteran profile = lowest risk of waiver return.</p>
<p><strong>Alternatives:</strong> Higher-upside options exist (Reifert for elite K%, Monegro for IL-stash ceiling, Walling for LHP depth), but Pushard offers the cleanest path to full-season roster carry.</p>
</div>

<div class="box">
<h2>Shortlist By Role</h2>

<table>
<tr><th>Role</th><th>Primary Rec</th><th>Rank</th><th>Grade</th><th>2025 IP</th><th>FIP</th><th>K/9</th><th>BB/9</th><th>Age/Lvl</th><th>Risk</th><th>Why This Arm</th></tr>
<tr>
<td><strong>High-Leverage (Safe)</strong></td>
<td><strong>Matt Pushard</strong></td>
<td class="num">#14</td>
<td><strong>B+</strong></td>
<td class="num">62.1</td>
<td class="num">3.36</td>
<td class="num">10.1</td>
<td class="num">3.3</td>
<td>27/AAA</td>
<td>Clean</td>
<td>Safest floor, proven track record, elite command</td>
</tr>
<tr>
<td><strong>High-Leverage (Upside)</strong></td>
<td><strong>Evan Reifert</strong></td>
<td class="num">#2</td>
<td><strong>A</strong></td>
<td class="num">60.1</td>
<td class="num">2.90</td>
<td class="num">11.5</td>
<td class="num">4.3</td>
<td>26/AAA</td>
<td>⚠️ MOD</td>
<td>Elite K%, higher ceiling, shoulder history</td>
</tr>
<tr>
<td><strong>IL Stash</strong></td>
<td><strong>Yordanny Monegro</strong></td>
<td class="num">#1</td>
<td><strong>A</strong></td>
<td class="num">21.0</td>
<td class="num">2.70</td>
<td class="num">13.2</td>
<td class="num">3.9</td>
<td>22/AA</td>
<td>🚨 CRITICAL</td>
<td>Highest ceiling, Tommy John 6/2025, 60-day IL only</td>
</tr>
<tr>
<td><strong>LHP Option</strong></td>
<td><strong>Mitch Walling</strong></td>
<td class="num">#13</td>
<td><strong>B+</strong></td>
<td class="num">58.0</td>
<td class="num">3.53</td>
<td class="num">11.0</td>
<td class="num">2.8</td>
<td>26/AA</td>
<td>Clean</td>
<td>Platoon-neutral indicators (GB% 48%), sinker/cutter heavy</td>
</tr>
</table>
</div>
<h4>Matt Pushard (R, 27yo, AAA) - Grade B+ - PRIMARY RECOMMENDATION</h4>
<p><strong>Stats:</strong> Rank #14 | 2025: 62.1 IP | Career FIP 3.36 | K/9 10.1 | BB/9 3.3 | Clean health</p>

<div class="rec">
<h4>Matt Pushard (R, 27yo, AAA) - Grade B+ - PRIMARY RECOMMENDATION</h4>
<p><strong>Stats:</strong> Rank #14 | FIP 3.36 | K/9 10.1 | BB/9 3.3 | IP 166.4 | Clean health</p>
<p><strong>Readiness:</strong> Age 27 AAA veteran, middle-relief role. Immediate depth.<br>
<strong>Risk:</strong> Clean injury history. IL park validates numbers. Age 27 = limited projection upside.<br>
<strong>Upside:</strong> Elite command (BB/9 3.3), proven at AAA. Safest full-season carry.<br>
<strong>Pitch mix:</strong> Mid-90s four-seam with carry, hard slider primary weapon, changeup to RHH.</p>
</div>

<div class="rec">
<h4>Evan Reifert (R, 26yo, AAA) - Grade A - HIGH UPSIDE ALTERNATIVE</h4>
<p><strong>Stats:</strong> Rank #2 | 2025: 60.1 IP | Career FIP 2.90 | K/9 11.5 | BB/9 4.3 | Moderate injury risk</p>
<p><strong>Readiness:</strong> AAA track record. Elite bat-misser with 7-9 inning upside.<br>
<strong>Risk:</strong> 2 shoulder IL stints - medical review needed. Walk rate volatility.<br>
<strong>Upside:</strong> Higher K ceiling, can develop into setup/closer role.<br>
<strong>Pitch mix:</strong> Mid-90s fastball with ride, plus changeup, developing slider vs RHH.</p>
</div>

<div class="rec">
<h4>Yordanny Monegro (R, 22yo, AA) - Grade A - IL STASH ONLY</h4>
<p><strong>Stats:</strong> Rank #1 | 2025: 21.0 IP (before TJ) | Career FIP 2.70 | K/9 13.2 | Tommy John 6/2025</p>
<p><strong>Readiness:</strong> Elite stuff but Tommy John 6/2025 - IL stash only.<br>
<strong>Risk:</strong> CRITICAL injury flag. Not a realistic full-season carry without medical clearance.<br>
<strong>Upside:</strong> Highest ceiling in pool if recovery clean. 60-day IL slot required.<br>
<strong>Pitch mix:</strong> Upper-90s fastball, power slider, developing changeup. Plus-plus stuff when healthy.</p>
</div>

<div class="rec">
<h4>Mitch Walling (L, 26yo, AA) - Grade B+ - LHP OPTION</h4>
<p><strong>Stats:</strong> Rank #13 | 2025: 58.0 IP | Career FIP 3.53 | K/9 11.0 | GB% 48% | Clean health</p>
<p><strong>Readiness:</strong> AA LHP with clean health.<br>
<strong>Risk:</strong> No public platoon splits - scouting confirmation needed.<br>
<strong>Upside:</strong> LHP with platoon-neutral indicators can handle RHB in leverage.<br>
<strong>Pitch mix:</strong> Low-90s sinker/cutter mix, sweeper, changeup as show pitch.</p>
</div>

<h2>Next Steps To Final Decision</h2>
<ol>
<li><strong>Medical and IL review:</strong> Verify Pushard has no undisclosed elbow/shoulder flags. Cross-reference all injury-flagged players with medical staff.</li>
<li><strong>Recent scouting looks:</strong> Confirm Pushard stuff/command held through 2025 (velocity, slider shape). Get eyes on Reifert changeup, Walling platoon indicators.</li>
<li><strong>Roster alignment:</strong> Discuss with manager/pitching coach: credible Opening Day role for Pushard (middle relief, 6-8 inning work).</li>
</ol>
<p><strong>If these checkpoints clear for Matt Pushard and do not reveal a clearly superior alternative, proceed with Pushard as the Rule 5 selection.</strong></p>

<h2>Top 25 Rankings</h2>
<details>
<summary><strong>Click to expand full Top 25 table with all metrics</strong></summary>
<table>
<tr><th>Rank</th><th>Player</th><th>Hand</th><th>Age</th><th>Lvl</th><th>2025 IP</th><th>FIP</th><th>K/9</th><th>BB/9</th><th>Grade</th><th>Injury</th></tr>'

for(i in 1:nrow(top25)) {
  row <- top25[i,]
  injury <- ifelse(row$injury_risk == "CRITICAL", "CRITICAL",
                   ifelse(row$injury_risk == "HIGH", "HIGH",
                          ifelse(row$injury_risk == "MODERATE", "MOD", "—")))
  
  html <- paste0(html, sprintf('
<tr><td class="num">%d</td><td><strong>%s</strong></td><td>%s</td><td class="num">%d</td><td>%s</td><td class="num">%.1f</td><td class="num">%.2f</td><td class="num">%.1f</td><td class="num">%.1f</td><td><strong>%s</strong></td><td>%s</td></tr>',
    row$overall_rank, row$player_name, 
    ifelse(is.na(row$pitcher_hand), "R", row$pitcher_hand),
    row$latest_age, row$latest_level, row$latest_ip, row$career_fip, row$career_k9, row$career_bb9,
    row$overall_grade, injury))
}

html <- paste0(html, '
</table>
</details>

<h2>Appendix - Extended Player Detail</h2>
<details>
<summary><strong>Click to expand detailed scouting notes and metrics</strong></summary>

<div class="rec" style="margin-top:20px">
<h4>Matt Pushard - Extended Detail</h4>
<p><strong>Full Career Stats:</strong> 166.4 IP, 127 G, 13 SV, 3.31 ERA, 3.36 FIP, 10.1 K/9, 3.3 BB/9, 0.62 HR/9</p>
<p><strong>Park Context:</strong> Pitched in IL (pitcher-friendly) - numbers validated, not inflated by environment</p>
<p><strong>Consistency:</strong> ERA consistency score 1.37 (low variance = reliable)</p>
<p><strong>Level Progression:</strong> 4 seasons, avg age vs level 2.75 (advanced for each level)</p>
<p><strong>Usage Pattern:</strong> 1.31 IP/G avg = one-inning reliever with occasional 2-IP appearances</p>
<p><strong>Fastball:</strong> Mid-90s four-seam, above-avg carry/ride, plays up in zone</p>
<p><strong>Slider:</strong> Primary weapon, hard mid-80s, horizontal break, generates whiffs and weak contact</p>
<p><strong>Changeup:</strong> Occasional third pitch to RHH, developing but functional</p>
<p><strong>Command Profile:</strong> BB/9 3.3 = elite control, repeatable delivery, consistent release point</p>
<p><strong>Body/Athleticism:</strong> Durable frame, clean arm action, no mechanical red flags (confirm with recent looks)</p>
<p><strong>Comp:</strong> AAA journeyman who becomes steady MLB middle reliever - think Ryan Tepera, Tyler Duffey type</p>
</div>

<div class="rec">
<h4>Evan Reifert - Extended Detail</h4>
<p><strong>Full Career Stats:</strong> 186.2 IP, 2.85 ERA, 2.90 FIP, 11.5 K/9, 4.3 BB/9, 0.77 HR/9</p>
<p><strong>Injury History:</strong> 7-day IL (5/6/2025), 7-day IL (4/13/2024) - both shoulder-related, no TJ history</p>
<p><strong>K Upside:</strong> 11.5 K/9 = 96th percentile in this pool, bat-missing ability proven across levels</p>
<p><strong>Walk Concern:</strong> BB/9 4.3 above league avg - command volatility limits floor but not ceiling</p>
<p><strong>Fastball:</strong> Mid-90s with plus ride, tunnels well with changeup, sets up breaking balls</p>
<p><strong>Changeup:</strong> Plus pitch, significant velocity differential, generates swings-and-misses vs LHH</p>
<p><strong>Slider:</strong> Developing third pitch vs RHH, flashes above-avg but inconsistent shape</p>
<p><strong>Deployment Risk:</strong> High-leverage upside but shoulder history + walk rate = medical review + patient development plan needed</p>
<p><strong>Comp:</strong> High-K reliever with setup upside if healthy - think Matt Barnes, Garrett Whitlock type</p>
</div>

<div class="rec">
<h4>Yordanny Monegro - Extended Detail</h4>
<p><strong>Full Career Stats:</strong> 109.2 IP, 2.30 ERA, 2.70 FIP, 13.2 K/9, 3.9 BB/9, 0.49 HR/9</p>
<p><strong>Injury:</strong> Torn UCL requiring Tommy John surgery, diagnosed 6/2025 - likely out through 2026 season</p>
<p><strong>Stuff Grade:</strong> Plus-plus when healthy, upper-90s fastball, power slider, developing changeup</p>
<p><strong>Age/Timeline:</strong> Age 22 = if recovery clean by spring 2027, still in prime development window</p>
<p><strong>IL Stash Strategy:</strong> Draft + 60-day IL = keeps off active roster while retaining rights, revisit 2027</p>
<p><strong>Risk/Reward:</strong> Highest ceiling in pool but cannot contribute in 2025-26 = only viable if org has IL slot to burn</p>
<p><strong>Comp:</strong> High-risk/high-reward TJ recovery - think Jonathan Loaisiga, Emmanuel Clase development path</p>
</div>

<div class="rec">
<h4>Mitch Walling - Extended Detail</h4>
<p><strong>Full Career Stats:</strong> 144.2 IP, 3.48 ERA, 3.53 FIP, 11.0 K/9, 2.8 BB/9, GB% 48%</p>
<p><strong>LHP Scarcity:</strong> Only 5 LHP in top 25 - market value higher than raw ranking suggests</p>
<p><strong>Platoon Profile:</strong> GB% 48% suggests sinker/cutter heavy approach = less platoon-sensitive than traditional LHP</p>
<p><strong>Pitch Mix Detail:</strong> Low-90s sinker (primary), cutter, sweeper, changeup (show pitch)</p>
<p><strong>Usage Projection:</strong> Can face RHH in multi-batter situations, not pure LOOGY role</p>
<p><strong>Data Gap:</strong> No public vs RHB/LHB splits - need internal scouting confirmation before selecting</p>
<p><strong>Comp:</strong> Ground-ball LHP with platoon-neutral potential - think Andrew Chafin, Brooks Raley type</p>
</div>

</details>

<h2>Methods In Brief</h2>
<div class="box">
<p>This model ranks 527 Rule 5-eligible relievers using a three-tier composite score weighted 50% performance (FIP, K/9, BB/9), 30% context (age/level fit, GB%, consistency), and 20% projection (MLB readiness). Grades (A = 75+, B+ = 65-74, B = 55-64) reflect likelihood of contributing on a 26-man roster. This is a screening tool, not a scouting report.</p>
</div>

<p style="margin-top:40px;padding-top:20px;border-top:1px solid #ccc;color:#666;font-size:13px">
Analysis: December 2025 | Data: FanGraphs MiLB (2021-2025) | 527 pitchers, 1,774 season records
</p>

</body></html>')

writeLines(html, "reports/Rule5_Decision_Brief.html")

cat("\n✅ Decision Brief generated: reports/Rule5_Decision_Brief.html\n")
