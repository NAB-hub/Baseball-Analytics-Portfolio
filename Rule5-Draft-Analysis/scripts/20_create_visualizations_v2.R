# =============================================================================
# 20_create_visualizations_v2.R
# Professional visualizations with modern aesthetics for PDF report
# Enhanced branding, colors, and typography
# =============================================================================

library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(extrafont)

# Create output directory if needed
if (!dir.exists("output/plots")) {
  dir.create("output/plots", recursive = TRUE)
}

# Load data
top25 <- read.csv("output/top25_targets.csv", stringsAsFactors = FALSE)
overall <- read.csv("output/overall_rankings.csv", stringsAsFactors = FALSE)
lhp <- read.csv("output/top10_lhp_specialists.csv", stringsAsFactors = FALSE)

# =============================================================================
# ENHANCED COLOR PALETTE - Modern, Professional, Print-Friendly
# =============================================================================
rule5_colors <- list(
  # Primary brand colors
  primary_blue = "#0d47a1",      # Deep professional blue
  accent_orange = "#f57c00",      # Vibrant orange for highlights
  success_green = "#2e7d32",      # Rich green for positive metrics
  warning_red = "#c62828",        # Bold red for warnings
  
  # Pitcher type colors (distinct and accessible)
  multi_inning = "#1976d2",       # Blue - workhorses
  one_inning = "#ff6f00",         # Orange - specialists
  hybrid = "#388e3c",             # Green - versatile
  undetermined = "#757575",       # Gray - unclear
  
  # Grade colors (gradient)
  grade_a_plus = "#1b5e20",
  grade_a = "#388e3c",
  grade_b_plus = "#689f38",
  grade_b = "#afb42b",
  grade_c_plus = "#fbc02d",
  grade_c = "#f57f17",
  grade_d = "#e65100",
  
  # Neutral tones
  background = "#fafafa",
  text_dark = "#212121",
  text_light = "#757575",
  grid_light = "#e0e0e0"
)

# Custom theme for all plots
theme_rule5 <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text
      plot.title = element_text(face = "bold", size = base_size + 3, 
                                color = rule5_colors$text_dark, 
                                margin = margin(b = 8)),
      plot.subtitle = element_text(size = base_size, 
                                   color = rule5_colors$text_light,
                                   margin = margin(b = 12)),
      axis.title = element_text(size = base_size - 1, 
                                face = "bold", 
                                color = rule5_colors$text_dark),
      axis.text = element_text(size = base_size - 2, 
                               color = rule5_colors$text_light),
      
      # Legend
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 2),
      legend.position = "bottom",
      legend.box = "horizontal",
      
      # Grid
      panel.grid.major = element_line(color = rule5_colors$grid_light, size = 0.3),
      panel.grid.minor = element_blank(),
      
      # Background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

# =============================================================================
# 1. TOP 25 RANKINGS - Enhanced Bar Chart
# =============================================================================
p1 <- ggplot(top25, aes(x = reorder(player_name, composite_score), 
                        y = composite_score, fill = pitcher_type)) +
  geom_col(width = 0.75, alpha = 0.9) +
  geom_text(aes(label = overall_rank), 
            hjust = -0.3, size = 3, fontface = "bold", 
            color = rule5_colors$text_dark) +
  scale_fill_manual(
    values = c(
      "Multi-Inning" = rule5_colors$multi_inning,
      "One-Inning" = rule5_colors$one_inning,
      "Hybrid" = rule5_colors$hybrid,
      "Undetermined" = rule5_colors$undetermined
    ),
    name = "Pitcher Type"
  ) +
  labs(
    title = "Top 25 Rule 5 Draft Targets",
    subtitle = "FIP-Dominant Framework | Composite Score = Tier 1 (50%) + Tier 2 (30%) + Tier 3 (20%)",
    x = NULL,
    y = "Composite Score (0-100)"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                     breaks = seq(0, 100, 20)) +
  theme_rule5() +
  theme(
    axis.text.y = element_text(size = 9, hjust = 1),
    legend.position = "top"
  )

ggsave("output/plots/01_top25_rankings.png", p1, width = 12, height = 10, dpi = 300, bg = "white")
cat("✅ Created: Enhanced Top 25 rankings chart\n")

# =============================================================================
# 2. TIER BREAKDOWN SCATTER - Performance vs Projection
# =============================================================================
p2 <- ggplot(top25, aes(x = tier1_score, y = tier3_score, color = pitcher_type)) +
  geom_point(aes(size = tier2_score), alpha = 0.7, shape = 16) +
  geom_text(aes(label = overall_rank), size = 2.5, fontface = "bold", 
            color = "white", show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Multi-Inning" = rule5_colors$multi_inning,
      "One-Inning" = rule5_colors$one_inning,
      "Hybrid" = rule5_colors$hybrid,
      "Undetermined" = rule5_colors$undetermined
    ),
    name = "Pitcher Type"
  ) +
  scale_size_continuous(range = c(4, 14), name = "Tier 2 Score\n(Context)") +
  labs(
    title = "Performance vs. Projection Matrix",
    subtitle = "Tier 1 (FIP, K/9, BB/9) vs. Tier 3 (MLB Readiness, Stuff+, Upside)",
    x = "Tier 1 Score - Current Performance",
    y = "Tier 3 Score - Future Projection & MLB Readiness"
  ) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
  geom_vline(xintercept = 70, linetype = "dashed", color = rule5_colors$grid_light, size = 0.5) +
  geom_hline(yintercept = 70, linetype = "dashed", color = rule5_colors$grid_light, size = 0.5) +
  annotate("text", x = 95, y = 95, label = "Elite\nProspects", 
           size = 3, color = rule5_colors$success_green, fontface = "bold") +
  theme_rule5() +
  theme(legend.position = "right")

ggsave("output/plots/02_tier_scatter.png", p2, width = 11, height = 8, dpi = 300, bg = "white")
cat("✅ Created: Enhanced tier scatter plot\n")

# =============================================================================
# 3. PITCHER TYPE DISTRIBUTION - Modern Bar Chart
# =============================================================================
type_summary <- overall %>%
  group_by(pitcher_type) %>%
  summarise(
    count = n(),
    avg_score = mean(composite_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

p3 <- ggplot(type_summary, aes(x = reorder(pitcher_type, count), y = count, fill = pitcher_type)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0("n = ", count, "\nAvg: ", round(avg_score, 1))), 
            vjust = -0.5, size = 3.5, fontface = "bold", 
            color = rule5_colors$text_dark, lineheight = 0.9) +
  scale_fill_manual(
    values = c(
      "Multi-Inning" = rule5_colors$multi_inning,
      "One-Inning" = rule5_colors$one_inning,
      "Hybrid" = rule5_colors$hybrid,
      "Undetermined" = rule5_colors$undetermined
    )
  ) +
  labs(
    title = "Pitcher Type Distribution Across 527 Eligible Pitchers",
    subtitle = "Classification by innings per game and role projection",
    x = NULL,
    y = "Number of Pitchers"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_rule5() +
  theme(legend.position = "none")

ggsave("output/plots/03_pitcher_type_distribution.png", p3, width = 10, height = 7, dpi = 300, bg = "white")
cat("✅ Created: Enhanced pitcher type distribution\n")

# =============================================================================
# 4. TOP 10 LHP SPECIALISTS - Horizontal Bar Chart
# =============================================================================
if (nrow(lhp) > 0) {
  lhp_plot <- head(lhp, 10)
  
  p4 <- ggplot(lhp_plot, aes(x = reorder(player_name, composite_score), 
                             y = composite_score, fill = primary_role)) +
    geom_col(width = 0.75, alpha = 0.9) +
    geom_text(aes(label = paste0("#", lhp_rank)), 
              hjust = -0.3, size = 3.5, fontface = "bold", 
              color = rule5_colors$text_dark) +
    scale_fill_manual(
      values = c(
        "Multi-Inning" = rule5_colors$multi_inning,
        "One-Inning" = rule5_colors$one_inning,
        "Hybrid" = rule5_colors$hybrid
      ),
      name = "Pitcher Type"
    ) +
    labs(
      title = "Top 10 Left-Handed Pitcher Specialists",
      subtitle = "Elite LHP options for matchup situations and LOOGY roles",
      x = NULL,
      y = "Composite Score"
    ) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                       breaks = seq(0, 100, 20)) +
    theme_rule5() +
    theme(
      axis.text.y = element_text(size = 10),
      legend.position = "top"
    )
  
  ggsave("output/plots/04_lhp_specialists.png", p4, width = 11, height = 8, dpi = 300, bg = "white")
  cat("✅ Created: Enhanced LHP specialists chart\n")
}

# =============================================================================
# 5. GRADE DISTRIBUTION - Gradient Bar Chart
# =============================================================================
grade_counts <- overall %>%
  filter(!is.na(overall_grade)) %>%
  count(overall_grade) %>%
  mutate(
    overall_grade = factor(overall_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D")),
    grade_color = case_when(
      overall_grade == "A+" ~ rule5_colors$grade_a_plus,
      overall_grade == "A" ~ rule5_colors$grade_a,
      overall_grade == "B+" ~ rule5_colors$grade_b_plus,
      overall_grade == "B" ~ rule5_colors$grade_b,
      overall_grade == "C+" ~ rule5_colors$grade_c_plus,
      overall_grade == "C" ~ rule5_colors$grade_c,
      TRUE ~ rule5_colors$grade_d
    )
  )

p5 <- ggplot(grade_counts, aes(x = overall_grade, y = n, fill = overall_grade)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = n), vjust = -0.5, size = 4, fontface = "bold",
            color = rule5_colors$text_dark) +
  scale_fill_manual(values = setNames(grade_counts$grade_color, grade_counts$overall_grade)) +
  labs(
    title = "Overall Grade Distribution - All 527 Eligible Pitchers",
    subtitle = "Letter grades based on composite score: A (75+), B (65-74), C (45-64), D (<45)",
    x = "Overall Grade",
    y = "Number of Pitchers"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_rule5() +
  theme(legend.position = "none")

ggsave("output/plots/05_grade_distribution.png", p5, width = 10, height = 7, dpi = 300, bg = "white")
cat("✅ Created: Enhanced grade distribution chart\n")

# =============================================================================
# 6. TOP 15 SCORE HEATMAP - Modern Design
# =============================================================================
library(tidyr)

heatmap_data <- top25 %>%
  select(overall_rank, player_name, tier1_score, tier2_score, tier3_score, composite_score) %>%
  arrange(overall_rank) %>%
  head(15)

heatmap_long <- heatmap_data %>%
  pivot_longer(cols = c(tier1_score, tier2_score, tier3_score, composite_score),
               names_to = "metric",
               values_to = "score") %>%
  mutate(
    metric = factor(metric, 
                    levels = c("tier1_score", "tier2_score", "tier3_score", "composite_score"),
                    labels = c("Tier 1\nPerformance", "Tier 2\nContext", 
                              "Tier 3\nProjection", "Composite\nScore")),
    score_label = round(score, 1)
  )

p6 <- ggplot(heatmap_long, aes(x = metric, y = reorder(player_name, -overall_rank), fill = score)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = score_label), color = "white", fontface = "bold", size = 3.5) +
  scale_fill_gradient2(
    low = rule5_colors$warning_red, 
    mid = "#fdd835", 
    high = rule5_colors$success_green,
    midpoint = 65, 
    limits = c(0, 100),
    name = "Score"
  ) +
  labs(
    title = "Top 15 Targets - Comprehensive Score Breakdown",
    subtitle = "Tier performance across all analytical dimensions",
    x = NULL,
    y = NULL
  ) +
  theme_rule5() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave("output/plots/06_top15_heatmap.png", p6, width = 11, height = 9, dpi = 300, bg = "white")
cat("✅ Created: Enhanced top 15 heatmap\n")

# =============================================================================
# 7. TOP 10 METRICS RADAR - Faceted Display
# =============================================================================
top10_metrics <- top25 %>%
  head(10) %>%
  select(overall_rank, player_name, career_era, career_fip, career_k9, career_bb9) %>%
  pivot_longer(cols = c(career_era, career_fip, career_k9, career_bb9),
               names_to = "metric",
               values_to = "value") %>%
  mutate(
    metric = factor(metric,
                    levels = c("career_fip", "career_k9", "career_bb9", "career_era"),
                    labels = c("FIP (lower better)", "K/9 (higher better)", 
                              "BB/9 (lower better)", "ERA (lower better)")),
    player_name_short = substr(player_name, 1, 15)  # Shorten for display
  )

p7 <- ggplot(top10_metrics, aes(x = reorder(player_name_short, -overall_rank), 
                                y = value, fill = metric)) +
  geom_col(width = 0.8, alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "FIP (lower better)" = rule5_colors$primary_blue,
      "K/9 (higher better)" = rule5_colors$success_green,
      "BB/9 (lower better)" = rule5_colors$accent_orange,
      "ERA (lower better)" = "#7b1fa2"
    )
  ) +
  labs(
    title = "Top 10 - Key Performance Metrics Comparison",
    subtitle = "Career MiLB statistics (level-weighted: AAA 3x, AA 2x, A+ 0.3x)",
    x = NULL,
    y = "Metric Value",
    fill = "Metric"
  ) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  theme_rule5() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = rule5_colors$background, color = NA),
    legend.position = "none"
  )

ggsave("output/plots/07_top10_metrics.png", p7, width = 12, height = 9, dpi = 300, bg = "white")
cat("✅ Created: Enhanced top 10 metrics comparison\n")

cat("\n✅ All enhanced visualizations created successfully!\n")
cat("   Output directory: output/plots/\n")
cat("   7 professional-grade charts generated for PDF report\n")
cat("   Resolution: 300 DPI (print-ready)\n")
cat("   Color palette: Accessible and brand-consistent\n")
