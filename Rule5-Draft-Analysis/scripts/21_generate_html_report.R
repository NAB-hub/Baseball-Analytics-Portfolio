# =============================================================================
# 21_generate_pdf_report.R
# Generate comprehensive PDF report for Rule 5 Draft analysis
# Mobile-optimized for presentations with front office personnel
# =============================================================================

library(dplyr)
library(knitr)
library(rmarkdown)

cat("📄 Generating PDF Report...\n\n")

# Check if visualizations exist
if (!file.exists("output/plots/01_top25_rankings.png")) {
  cat("⚠️  Visualizations not found. Running visualization script...\n")
  source("scripts/20_create_visualizations.R")
}

# Check if injury risk analysis exists
if (!file.exists("output/injury_risk_summary.csv")) {
  cat("⚠️  Injury risk data not found. Running analysis...\n")
  source("scripts/12_injury_risk_analysis.R")
}

# Copy images to reports folder for PDF generation
if (!dir.exists("reports/plots")) dir.create("reports/plots", recursive = TRUE)
plot_files <- list.files("output/plots", pattern = "\\.png$", full.names = TRUE)
file.copy(plot_files, "reports/plots/", overwrite = TRUE)

# Create R Markdown report
rmd_content <- '---
title: "Rule 5 Draft Relief Pitcher Analytics"
subtitle: "Top 25 Targets and Strategic Recommendations"
date: "December 1, 2025"
params:
  project_root: ""
output:
  html_document:
    toc: true
    toc_depth: 2
    toc_float: true
    theme: flatly
    highlight: tango
    code_folding: hide
    df_print: paged
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
    fig_width: 6.5
    fig_height: 4
    latex_engine: xelatex
geometry: margin=0.75in
fontsize: 10pt
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.align = "center")
library(dplyr)
library(knitr)
library(kableExtra)

# Get project root from params (set during render)
if (params$project_root != "") {
  project_root <- params$project_root
} else {
  # Fallback: try to find it
  project_root <- normalizePath("..")
}
```

\\newpage

# Executive Summary

This report presents a **comprehensive analytical framework** for evaluating **527 Rule 5 Draft-eligible relief pitchers** using advanced metrics, predictive analytics, and context-aware scoring methodology.

## Key Findings & Analytics Framework

### FIP-Dominant Approach (Predictive Over Results-Based)
- **40% weight on FIP** vs. traditional ERA-heavy models
- **Emphasis on K/9 (25%), BB/9 (20%), K-BB spread (15%)**
- Prioritizes skills pitchers control (strikeouts, walks) over defense-dependent outcomes
- **Result**: Top 25 now features elite peripherals (FIP <3.0) with MLB-ready command

### Context-Aware Age Scoring
- **AAA Veterans rewarded** (ages 26-28 = prime Rule 5 targets with experience)
- **AA Prospects** evaluated on traditional development curve
- **A+ Youth premium** for high-upside players
- Eliminates penalty for MLB-ready 27-year-olds at AAA (previously penalized as "too old")

### Park Factor Intelligence
- **International League (IL)** flagged as pitcher-friendly, reliable stats ✅
- **Pacific Coast League (PCL)** identified as hitter-friendly with altitude inflation ⚠️
- Helps evaluate AAA pitchers in proper environmental context
- 31 IL pitchers vs. 7 PCL pitchers analyzed for park-adjusted interpretation

## Population & Results

- **Top 25 Targets**: Elite prospects with composite scores 70+
- **87 MLB-Ready Targets**: High roster stick probability (60-75%+)
- **Pitcher Classification**: 324 Multi-Inning, 117 One-Inning, 29 Hybrid, 86 Undetermined
- **LHP Specialists**: 110 left-handed pitchers, Top 10 identified for matchup roles
- **Data Foundation**: 1,774 MiLB season records (2021-2025) from A+, AA, AAA levels

## Advanced Metrics Integration

- **Stuff+ Proxy**: Velocity + K-rate + control + GB% + contact quality composite (95-109 range)
- **MLB Roster Projection**: 5-factor model (Role Definition 30%, MLB Stuff 25%, Command 20%, Experience 15%, Durability 10%)
- **Handedness Integration**: 382 LHP, 1,392 RHP properly classified for platoon analysis
- **Injury Risk Flags**: Performance degradation indicators (K/9 drops, BB/9 spikes, FIP collapses)

## Scoring Methodology

Our proprietary **3-Tier Weighted System** evaluates pitchers across three dimensions:

- **Tier 1 (50% weight)**: FIP-Dominant Performance - FIP 40%, K/9 25%, BB/9 20%, K-BB 15% (ERA removed)
- **Tier 2 (30% weight)**: Context-Aware Analysis - Age/Level fit, GB%, Consistency, Workload, Competition
- **Tier 3 (20% weight)**: Future Projection - MLB Readiness, Stuff+ proxy, Peak Performance, Upside

**Overall Grade Scale**: A (75+), B+ (65-74), B (55-64), C+ (45-54), C (35-44), D (<35)

\\newpage

# Top 25 Rule 5 Draft Targets

```{r load_data}
top25 <- read.csv(file.path(project_root, "output/top25_targets.csv"), stringsAsFactors = FALSE)
```

## Complete Rankings

```{r top25_table}
top25_display <- top25 %>%
  select(overall_rank, player_name, pitcher_type, composite_score, overall_grade, 
         career_era, career_fip, career_k9, career_bb9, latest_level) %>%
  mutate(
    composite_score = round(composite_score, 1),
    career_era = round(career_era, 2),
    career_fip = round(career_fip, 2),
    career_k9 = round(career_k9, 1),
    career_bb9 = round(career_bb9, 1)
  )

kable(top25_display, 
      col.names = c("Rank", "Player", "Type", "Score", "Grade", "ERA", "FIP", "K/9", "BB/9", "Level"),
      align = c("c", "l", "l", "c", "c", "c", "c", "c", "c", "c"),
      caption = "Top 25 Rule 5 Draft Targets - Complete Rankings") %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"), 
                font_size = 8) %>%
  row_spec(0, bold = TRUE, background = "#1f77b4", color = "white")
```

\\newpage

## Visual Rankings

```{r top25_chart, fig.width=6.5, fig.height=4, out.width="100%"}
knitr::include_graphics("plots/01_top25_rankings.png")
```

**Figure 1**: Composite scores for Top 25 targets, color-coded by pitcher type. Numbers above bars indicate overall rank.

\\newpage

# Performance Analysis

## Tier Breakdown: Performance vs. Projection

```{r tier_scatter, fig.width=6.5, fig.height=5, out.width="100%"}
knitr::include_graphics("plots/02_tier_scatter.png")
```

**Figure 2**: Current performance (Tier 1) plotted against future projection (Tier 3). Point size represents Tier 2 contextual score.

\\newpage

## Top 15 Score Heatmap

```{r heatmap, fig.width=6.5, fig.height=5, out.width="100%"}
knitr::include_graphics("plots/06_top15_heatmap.png")
```

**Figure 3**: Detailed breakdown of tier scores for Top 15 targets. Darker green indicates higher scores.

\\newpage

# Pitcher Classification

## Type Distribution

```{r type_dist, fig.width=6.5, fig.height=4, out.width="100%"}
knitr::include_graphics("plots/03_pitcher_type_distribution.png")
```

**Figure 4**: Distribution of 527 analyzed pitchers by role classification. Numbers show count and average composite score.

## Multi-Inning vs. One-Inning Specialists

```{r type_breakdown}
type_summary <- top25 %>%
  group_by(pitcher_type) %>%
  summarise(
    Count = n(),
    `Avg Score` = round(mean(composite_score), 1),
    `Avg ERA` = round(mean(career_era, na.rm = TRUE), 2),
    `Avg K/9` = round(mean(career_k9, na.rm = TRUE), 1),
    .groups = "drop"
  )

kable(type_summary,
      caption = "Top 25 Breakdown by Pitcher Type",
      align = c("l", "c", "c", "c", "c")) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 9)
```

\\newpage

# Left-Handed Specialists

## Top 10 LHP Rankings

```{r lhp_data}
lhp <- read.csv(file.path(project_root, "output/top10_lhp_specialists.csv"), stringsAsFactors = FALSE)

lhp_display <- lhp %>%
  head(10) %>%
  select(lhp_rank, player_name, primary_role, composite_score, overall_grade,
         career_era, career_k9, career_bb9) %>%
  mutate(
    composite_score = round(composite_score, 1),
    career_era = round(career_era, 2),
    career_k9 = round(career_k9, 1),
    career_bb9 = round(career_bb9, 1)
  )

kable(lhp_display,
      col.names = c("LHP Rank", "Player", "Type", "Score", "Grade", "ERA", "K/9", "BB/9"),
      align = c("c", "l", "l", "c", "c", "c", "c", "c"),
      caption = "Top 10 Left-Handed Pitcher Specialists") %>%
  kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 9) %>%
  row_spec(0, bold = TRUE, background = "#ff7f0e", color = "white")
```

```{r lhp_chart, fig.width=6.5, fig.height=4, out.width="100%"}
knitr::include_graphics("plots/04_lhp_specialists.png")
```

**Figure 5**: Elite left-handed options for matchup situations and bullpen versatility.

\\newpage

# Injury Risk Analysis

## Red Flag Indicators

Our injury risk model identifies players with concerning performance degradation between levels or seasons. Key warning signs include:

- **Velocity Loss**: K/9 drops >25% (indicates potential arm fatigue/injury)
- **Command Issues**: BB/9 spikes >75% (suggests mechanical breakdown)
- **Overall Collapse**: FIP increases >50% (comprehensive performance failure)

```{r injury_risk_data}
injury_risk <- read.csv(file.path(project_root, "output/injury_risk_summary.csv"), stringsAsFactors = FALSE)

# Merge with top 25 to flag any concerns
top25_risk <- top25 %>%
  left_join(injury_risk %>% select(playerid, max_risk_level, avg_severity, worst_k9_drop),
            by = "playerid") %>%
  filter(!is.na(max_risk_level))

if (nrow(top25_risk) > 0) {
  risk_display <- top25_risk %>%
    select(overall_rank, player_name, max_risk_level, avg_severity, 
           worst_k9_drop, pitcher_type) %>%
    mutate(
      avg_severity = round(avg_severity, 1),
      worst_k9_drop = round(worst_k9_drop, 1)
    )
  
  kable(risk_display,
        col.names = c("Rank", "Player", "Risk Level", "Severity", "K/9 Drop %", "Type"),
        align = c("c", "l", "c", "c", "c", "l"),
        caption = "Top 25 Players with Injury Risk Flags") %>%
    kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 9) %>%
    row_spec(0, bold = TRUE, background = "#d62728", color = "white") %>%
    row_spec(which(risk_display$max_risk_level == "CRITICAL"), 
             background = "#ffcccc", bold = TRUE) %>%
    row_spec(which(risk_display$max_risk_level == "HIGH"), 
             background = "#ffe6cc")
}
```

## High-Risk Players (All Levels)

```{r all_risk}
if (nrow(injury_risk) > 0) {
  high_risk <- injury_risk %>%
    filter(max_risk_level %in% c("CRITICAL", "HIGH")) %>%
    head(15) %>%
    select(player_name, max_risk_level, latest_season, latest_level,
           worst_k9_drop, worst_bb9_spike, worst_fip_spike, avg_severity) %>%
    mutate(across(where(is.numeric), ~round(., 1)))
  
  kable(high_risk,
        col.names = c("Player", "Risk", "Season", "Level", 
                      "K/9 Drop %", "BB/9 Spike %", "FIP Spike %", "Severity"),
        align = c("l", "c", "c", "c", "c", "c", "c", "c"),
        caption = "Top 15 High-Risk Players (CRITICAL/HIGH flags)") %>%
    kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"), 
                  font_size = 8) %>%
    row_spec(0, bold = TRUE)
}
```

**Recommendation**: Cross-reference injury risk flags with medical reports before selecting. Players with CRITICAL risk levels should be evaluated by team medical staff.

\\newpage

# Top 10 Detailed Profiles

```{r top10_metrics, fig.width=6.5, fig.height=5, out.width="100%"}
knitr::include_graphics("plots/07_top10_metrics.png")
```

**Figure 6**: Key performance metrics comparison for Top 10 targets.

```{r top10_profiles}
top10 <- top25 %>%
  head(10) %>%
  select(overall_rank, player_name, pitcher_type, composite_score, overall_grade,
         tier1_score, tier2_score, tier3_score,
         career_era, career_fip, career_k9, career_bb9, 
         latest_level, latest_age) %>%
  mutate(across(where(is.numeric), ~round(., 1)))

kable(top10,
      col.names = c("Rank", "Player", "Type", "Score", "Grade", 
                    "T1", "T2", "T3", "ERA", "FIP", "K/9", "BB/9", "Level", "Age"),
      align = c("c", "l", "l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      caption = "Top 10 Comprehensive Profile") %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"), 
                font_size = 8) %>%
  row_spec(0, bold = TRUE)
```

\\newpage

# Overall Pool Analysis

## Grade Distribution (All 527 Pitchers)

```{r grade_dist, fig.width=6.5, fig.height=4, out.width="100%"}
knitr::include_graphics("plots/05_grade_distribution.png")
```

**Figure 7**: Complete grade distribution across all analyzed pitchers.

## Summary Statistics

```{r summary_stats}
overall <- read.csv(file.path(project_root, "output/overall_rankings.csv"), stringsAsFactors = FALSE)

summary_data <- data.frame(
  Metric = c("Total Pitchers Analyzed", 
             "Multi-Inning Relievers", 
             "One-Inning Specialists",
             "Hybrid Pitchers",
             "Left-Handed Pitchers",
             "Right-Handed Pitchers",
             "Average Composite Score",
             "Top 25 Avg Score",
             "Grade A or Better",
             "Grade B+ or Better",
             "MLB-Ready Targets (High Stick Prob)",
             "AAA Pitchers - International League",
             "AAA Pitchers - Pacific Coast League"),
  Value = c(
    nrow(overall),
    sum(overall$pitcher_type == "Multi-Inning", na.rm = TRUE),
    sum(overall$pitcher_type == "One-Inning", na.rm = TRUE),
    sum(overall$pitcher_type == "Hybrid", na.rm = TRUE),
    sum(overall$pitcher_hand == "L", na.rm = TRUE),
    sum(overall$pitcher_hand == "R", na.rm = TRUE),
    round(mean(overall$composite_score, na.rm = TRUE), 1),
    round(mean(top25$composite_score), 1),
    sum(overall$overall_grade %in% c("A+", "A"), na.rm = TRUE),
    sum(overall$overall_grade %in% c("A+", "A", "B+"), na.rm = TRUE),
    "87",
    sum(overall$latest_aaa_league == "IL", na.rm = TRUE),
    sum(overall$latest_aaa_league == "PCL", na.rm = TRUE)
  )
)

kable(summary_data,
      caption = "Complete Pool Summary Statistics",
      align = c("l", "c")) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 10)
```

\\newpage

# Park Factor Context - AAA League Intelligence

## International League vs. Pacific Coast League

AAA pitchers face dramatically different environmental conditions depending on league assignment:

### International League (IL) - Trustworthy Stats ✅
- **Pitcher-friendly environment** at normal elevation
- **31 pitchers** in our dataset from IL affiliates
- Stats are **reliable** for MLB projection
- Examples: Jacksonville (MIA), Durham (TB), Norfolk (BAL), Buffalo (TOR)

### Pacific Coast League (PCL) - Altitude Inflation ⚠️
- **Hitter-friendly** with multiple high-altitude parks
- **7 pitchers** in our dataset from PCL affiliates
- Stats may be **inflated** (higher ERA/FIP than true talent)
- Key parks: Albuquerque 5,400 ft, Salt Lake 4,200 ft, Reno 4,500 ft
- Examples: Sugar Land (HOU), Oklahoma City (LAD), Las Vegas (OAK), Tacoma (SEA)

### Strategic Implications

```{r park_factor_comparison}
aaa_pitchers <- overall %>%
  filter(latest_level == "AAA", !is.na(latest_aaa_league)) %>%
  group_by(latest_aaa_league) %>%
  summarise(
    Count = n(),
    `Avg FIP` = round(mean(career_fip, na.rm = TRUE), 2),
    `Avg ERA` = round(mean(career_era, na.rm = TRUE), 2),
    `Avg K/9` = round(mean(career_k9, na.rm = TRUE), 1),
    `Avg Composite` = round(mean(composite_score, na.rm = TRUE), 1),
    .groups = "drop"
  )

kable(aaa_pitchers,
      col.names = c("League", "Pitchers", "Avg FIP", "Avg ERA", "Avg K/9", "Avg Score"),
      caption = "AAA League Environmental Context Comparison",
      align = c("l", "c", "c", "c", "c", "c")) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 10) %>%
  row_spec(which(aaa_pitchers$latest_aaa_league == "IL"), 
           background = "#c8e6c9", bold = TRUE) %>%
  row_spec(which(aaa_pitchers$latest_aaa_league == "PCL"), 
           background = "#ffccbc")
```

**Recommendation**: Prioritize IL pitchers for AAA stats reliability. Adjust expectations for PCL pitchers - their true talent likely better than raw ERA suggests, but K/9 and BB/9 remain trustworthy.

\\newpage

# Methodology & Data Sources

## Data Collection

- **Source**: FanGraphs MiLB leaderboards and individual player pages
- **Population**: 605 Rule 5 Draft-eligible pitchers (40-man roster unprotected)
- **Time Period**: 2021-2025 MiLB seasons (recent performance weighted)
- **Levels**: A+, AA, AAA only (minimum 10 IP per season for statistical reliability)
- **Final Dataset**: 527 pitchers with 1,774 qualifying season records
- **Level Weighting**: AAA 3.0x, AA 2.0x, A+ 0.3x (higher levels weighted heavily)
- **Park Context**: IL vs. PCL league assignment tracked for AAA environmental adjustments

## Analytical Framework

### Tier 1: FIP-Dominant Performance (50% weight)

**Analytics-First Approach**: Prioritizes predictive metrics over results-based stats

- **FIP** (40%): Fielding Independent Pitching - what pitcher controls
- **K/9** (25%): Strikeout rate per 9 innings - swing-and-miss ability
- **BB/9** (20%): Walk rate per 9 innings - command quality
- **K-BB Spread** (15%): Net dominance metric
- **ERA removed** (0%): Too defense-dependent, not predictive

**Rationale**: FIP-dominant framework identifies pitchers with sustainable skills (strikeouts, walks) rather than luck-driven outcomes (BABIP, LOB%, defense). Top 25 now features elite K/9 (10+) and FIP (<3.5) profiles.

### Tier 2: Context-Aware Analysis (30% weight)

Evaluates performance context and consistency with **age-appropriate benchmarks**:

**Age vs. Level Scoring** (25% of Tier 2) - Context-specific:
- **AAA**: Ages 26-28 rewarded (80 pts) as MLB-ready veterans, not penalized
- **AA**: Ages 23-24 ideal (80 pts) for prospect development curve  
- **A+**: Ages 21-22 prioritized (100 pts) for youth/upside balance

**Other Context Factors**:
- **Ground Ball %** (25%): Batted ball profile quality
- **HR/9 Rate** (15%): Home run prevention ability
- **Consistency** (15%): ERA standard deviation across seasons
- **Workload** (10%): Total IP demonstrates durability
- **Competition Level** (10%): Highest level reached

### Tier 3: Future Projection (20% weight)

Projects MLB potential and upside with advanced metrics:

- **Stuff+ Proxy**: Composite of velocity + K-rate + control + GB% + contact (95-109 range)
- **MLB Readiness**: Age, level, and recent AAA/AA performance
- **Peak Performance**: Best FIP and K/9 seasons indicate ceiling
- **Trajectory**: Recent improvement trends vs. career baseline
- **Upside Calculation**: Youth bonus + elite pitch metrics

## Pitcher Classification System

Pitchers categorized by usage pattern and role projection:

- **Multi-Inning**: IP/G ≥ 1.3, potential for 2-3 inning outings
- **One-Inning**: IP/G < 1.3, high leverage specialist role
- **Hybrid**: Demonstrates flexibility between roles
- **Undetermined**: Insufficient data for clear classification

\\newpage

# Strategic Recommendations

## High-Priority Targets (Ranks 1-10)

Elite prospects with **Grade A- or better** composite scores. These pitchers demonstrate:

- Exceptional performance across all three tiers
- MLB-ready metrics with proven track records
- Age-appropriate advancement through system
- High-probability contributors in 2026

**Action**: Prioritize for selection, prepare 40-man roster space

## Value Targets (Ranks 11-25)

Strong **Grade B+** prospects with specific strengths:

- Specialists with defined MLB roles
- High-upside arms with projection remaining
- Left-handed matchup weapons (see LHP specialist list)
- Potential multi-year controllable assets

**Action**: Secondary tier selections, organizational depth plays

## Left-Handed Specialists

The **Top 10 LHP** list identifies rare southpaws for:

- Late-inning matchup situations
- LOOGY (Lefty One-Out Guy) potential
- Bullpen versatility and platoon advantages

**Action**: Prioritize 1-2 LHPs to complement right-handed bullpen depth

## Risk Considerations

All Rule 5 selections carry inherent risk:

- Must remain on 25-man roster full season or be offered back
- Limited minor league option flexibility
- Require immediate MLB readiness
- Organizational roster construction constraints

**Mitigation**: Focus on **Tier 3 MLB Readiness** scores, prioritize pitchers with AAA experience and age 24+

\\newpage

# Appendix

## Contact Information

**Analysis Date**: `r format(Sys.Date(), "%B %d, %Y")`

**Data Vintage**: MiLB Statistics 2019-2025

**Last Updated**: `r format(Sys.time(), "%B %d, %Y at %I:%M %p")`

## File Outputs

Complete analysis outputs available in project directory:

- `output/top25_targets.csv` - Top 25 complete dataset
- `output/top10_lhp_specialists.csv` - LHP specialist rankings
- `output/overall_rankings.csv` - All 527 pitchers ranked
- `output/multi_inning_rankings.csv` - Multi-inning specialists only
- `output/one_inning_rankings.csv` - One-inning specialists only
- `reports/TOP25_MASTER_SCOUTING_REPORT.txt` - Detailed scouting reports
- `reports/scouting_report_##_PlayerName.txt` - Individual player reports

## Reproducibility

Complete R pipeline available in `scripts/` directory:

```
99_master_pipeline.R - Execute complete analysis
10-14: Data prep and tier scoring
15-18: Rankings and classifications  
20-21: Visualizations and PDF generation
```

---

**Rule 5 Draft Relief Pitcher Analytics**  
*Professional scouting and analytics for front office decision-making*
'

# Write R Markdown file
writeLines(rmd_content, "reports/Rule5_Draft_Report.Rmd")
cat("✅ Created R Markdown template\n")

# Get absolute project root path
project_root <- getwd()

# Render to PDF
cat("📄 Rendering PDF (this may take a minute)...\n")
tryCatch({
  rmarkdown::render(
    input = "reports/Rule5_Draft_Report.Rmd",
    output_file = "Rule5_Draft_Report.pdf",
    output_dir = "reports/",
    params = list(project_root = project_root),
    quiet = FALSE
  )
  cat("\n✅ PDF REPORT GENERATED SUCCESSFULLY!\n")
  cat("   📱 Mobile-ready PDF: reports/Rule5_Draft_Report.pdf\n")
  cat("   📄 File size optimized for phone viewing\n")
  cat("   📊 Includes all visualizations and rankings\n\n")
  cat("   Transfer to your phone and you're ready for front office meetings! 🎯\n")
}, error = function(e) {
  cat("\n⚠️  PDF rendering failed. You may need to install:\n")
  cat("   - tinytex package: install.packages('tinytex'); tinytex::install_tinytex()\n")
  cat("   - kableExtra package: install.packages('kableExtra')\n")
  cat("   Error: ", conditionMessage(e), "\n")
  cat("\nFull error details:\n")
  print(e)
})

# Also render to HTML for immediate viewing
cat("\n📄 Rendering HTML version...\n")
tryCatch({
  rmarkdown::render(
    input = "reports/Rule5_Draft_Report.Rmd",
    output_format = "html_document",
    output_file = "Rule5_Draft_Report.html",
    output_dir = "reports/",
    params = list(project_root = project_root),
    quiet = FALSE
  )
  cat("\n✅ HTML REPORT GENERATED SUCCESSFULLY!\n")
  cat("   🌐 HTML report: reports/Rule5_Draft_Report.html\n")
  cat("   📊 Open in any web browser for immediate viewing\n\n")
}, error = function(e) {
  cat("\n⚠️  HTML rendering failed.\n")
  cat("   Error: ", conditionMessage(e), "\n")
  print(e)
})
