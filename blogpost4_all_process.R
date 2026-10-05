folders <- c(
  "data/raw",
  "data/processed",
  "scripts",
  "figures"
)

for (folder in folders) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

getwd()
list.dirs(recursive = TRUE)

data_url <- paste0(
  "https://www2.census.gov/programs-surveys/popest/",
  "datasets/2020-2025/state/totals/NST-EST2025-ALLDATA.csv"
)

raw_file <- "data/raw/NST-EST2025-ALLDATA.csv"

if (!file.exists(raw_file)) {
  download.file(
    url = data_url,
    destfile = raw_file,
    mode = "wb"
  )
  
  writeLines(
    c(
      paste("Source URL:", data_url),
      "Dataset vintage: 2025",
      paste("Downloaded at:", Sys.time())
    ),
    "data/raw/data_source.txt"
  )
}

population_raw <- read.csv(
  raw_file,
  colClasses = c(
    SUMLEV = "character",
    STATE = "character"
  )
)

dim(population_raw)

head(
  population_raw[
    , c("SUMLEV", "STATE", "NAME",
        "POPESTIMATE2020", "POPESTIMATE2025")
  ]
)

population_columns <- paste0("POPESTIMATE", 2020:2025)

population_states <- population_raw[
  population_raw$SUMLEV == "040" &
    !(population_raw$STATE %in% c("11", "72")),
  c("STATE", "NAME", population_columns)
]

population_states <- population_states[
  order(population_states$NAME),
]
rownames(population_states) <- NULL

stopifnot(
  nrow(population_states) == 50,
  length(unique(population_states$STATE)) == 50,
  setequal(population_states$NAME, state.name),
  all(vapply(population_states[population_columns],
             is.numeric, logical(1))),
  !anyNA(population_states[population_columns]),
  all(as.matrix(population_states[population_columns]) > 0)
)

write.csv(
  population_states,
  "data/processed/state_population_2020_2025.csv",
  row.names = FALSE
)

cat("Checks passed:", nrow(population_states), "states\n")

head(
  population_states[
    , c("NAME", "POPESTIMATE2020", "POPESTIMATE2025")
  ]
)

state_summary <- data.frame(
  state = population_states$NAME,
  population_2020 = population_states$POPESTIMATE2020,
  population_2025 = population_states$POPESTIMATE2025
)

state_summary$population_change <-
  state_summary$population_2025 - state_summary$population_2020

state_summary$growth_pct <-
  state_summary$population_change /
  state_summary$population_2020 * 100

state_summary$rank_growth <- rank(
  -state_summary$growth_pct,
  ties.method = "min"
)

state_summary$rank_change <- rank(
  -state_summary$population_change,
  ties.method = "min"
)

state_summary <- state_summary[
  order(-state_summary$growth_pct),
]
rownames(state_summary) <- NULL

write.csv(
  state_summary,
  "data/processed/state_population_summary.csv",
  row.names = FALSE
)

top10_growth <- head(state_summary, 10)

top10_change <- head(
  state_summary[order(-state_summary$population_change), ],
  10
)

display_columns <- c(
  "state", "population_change", "growth_pct",
  "rank_growth", "rank_change"
)

cat("\nTop 10 states by percentage growth:\n")
print(
  transform(top10_growth, growth_pct = round(growth_pct, 2))[
    , display_columns
  ],
  row.names = FALSE
)

cat("\nTop 10 states by population increase:\n")
print(
  transform(top10_change, growth_pct = round(growth_pct, 2))[
    , display_columns
  ],
  row.names = FALSE
)

# ---- Figure 1: Population growth across all 50 states ----

library(ggplot2)

plot1_data <- state_summary

plot1_data$state <- factor(
  plot1_data$state,
  levels = plot1_data$state[order(plot1_data$growth_pct)]
)

figure1 <- ggplot(
  plot1_data,
  aes(x = growth_pct, y = state)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_segment(
    aes(x = 0, xend = growth_pct, yend = state),
    color = "grey80",
    linewidth = 0.5
  ) +
  geom_point(
    color = "#2166AC",
    size = 2.4
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "Which states grew fastest?",
    subtitle = "Cumulative population change, July 1, 2020–July 1, 2025",
    x = "Population growth (%)",
    y = NULL,
    caption = paste(
      "Source: U.S. Census Bureau, Population Estimates Program, Vintage 2025.",
      "Includes 50 states; excludes Washington, D.C. and Puerto Rico.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9),
    plot.margin = margin(12, 18, 12, 12)
  )

print(figure1)

ggsave(
  filename = "figures/figure1_state_population_growth.png",
  plot = figure1,
  width = 9,
  height = 12,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ---- 6. Figure 2: Population paths for selected states ----

# 按累计增长率排序，选最高三个州和最低三个州
ranked_states <- state_summary[
  order(-state_summary$growth_pct),
]

selected_states <- c(
  head(ranked_states$state, 3),
  tail(ranked_states$state, 3)
)

print(selected_states)

# 将全部 50 州的数据转换为“每行一个州的一年”
population_long <- do.call(
  rbind,
  lapply(2020:2025, function(year) {
    
    population_this_year <-
      population_states[[paste0("POPESTIMATE", year)]]
    
    data.frame(
      state = population_states$NAME,
      year = year,
      population = population_this_year,
      population_index =
        population_this_year /
        population_states$POPESTIMATE2020 * 100
    )
  })
)
# 检查：50 州 × 6 年；所有州在 2020 年的指数都为 100
stopifnot(
  nrow(population_long) == 300,
  all(population_long$population_index[
    population_long$year == 2020
  ] == 100)
)

# 保存长表，方便复现
write.csv(
  population_long,
  "data/processed/state_population_long.csv",
  row.names = FALSE
)

# 筛选绘图用的六个州
plot2_data <- population_long[
  population_long$state %in% selected_states,
]

plot2_data$state <- factor(
  plot2_data$state,
  levels = selected_states
)

# 绘制趋势图
figure2 <- ggplot(
  plot2_data,
  aes(
    x = year,
    y = population_index,
    color = state,
    group = state
  )
) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "#0072B2", "#D55E00", "#009E73",
      "#CC79A7", "#E69F00", "#333333"
    )
  ) +
  scale_x_continuous(breaks = 2020:2025) +
  labs(
    title = "How did population growth unfold?",
    subtitle = "Three highest- and three lowest-growth states, 2020–2025",
    x = NULL,
    y = "Population index (2020 = 100)",
    color = NULL,
    caption = paste(
      "Source: U.S. Census Bureau, Population Estimates Program, Vintage 2025.",
      "July 1 estimates. States selected by cumulative growth among the 50 states.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 17),
    plot.caption = element_text(hjust = 0, size = 9)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

print(figure2)

ggsave(
  filename = "figures/figure2_population_trends.png",
  plot = figure2,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ---- 7. Figure 3: Growth rates versus population increases ----

library(ggrepel)

# 新增人口数换算为百万人
plot3_data <- state_summary
plot3_data$change_millions <-
  plot3_data$population_change / 1000000

# 标注两种排名各自的前三名，自动去除重复州名
label_states <- union(
  head(
    state_summary$state[order(-state_summary$growth_pct)],
    3
  ),
  head(
    state_summary$state[order(-state_summary$population_change)],
    3
  )
)

label_data <- plot3_data[
  plot3_data$state %in% label_states,
]

figure3 <- ggplot(
  plot3_data,
  aes(x = growth_pct, y = change_millions)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey65"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey65"
  ) +
  geom_point(
    color = "#2166AC",
    size = 2.8,
    alpha = 0.7
  ) +
  geom_point(
    data = label_data,
    color = "#D55E00",
    size = 3
  ) +
  geom_text_repel(
    data = label_data,
    aes(label = state),
    seed = 42,
    size = 3.8,
    box.padding = 0.6,
    point.padding = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.10, 0.15))
  ) +
  labs(
    title = "Fastest growth does not mean the most new residents",
    subtitle = "Population change across 50 states, July 2020–July 2025",
    x = "Cumulative population growth (%)",
    y = "Population change (millions)",
    caption = paste(
      "Source: U.S. Census Bureau, Population Estimates Program, Vintage 2025.",
      "Orange points: top three states by growth rate or population increase.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    plot.caption = element_text(hjust = 0, size = 9),
    plot.margin = margin(12, 18, 12, 12)
  )

print(figure3)

ggsave(
  filename = "figures/figure3_growth_vs_population_increase.png",
  plot = figure3,
  width = 10,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ---- 8. Check outputs and record the R environment ----

expected_figures <- c(
  "figures/figure1_state_population_growth.png",
  "figures/figure2_population_trends.png",
  "figures/figure3_growth_vs_population_increase.png"
)

# 确认三张图片存在，且文件不为空
stopifnot(
  all(file.exists(expected_figures)),
  all(file.info(expected_figures)$size > 0)
)

# 显示图片文件和大小
print(
  data.frame(
    file = basename(expected_figures),
    size_KB = round(file.info(expected_figures)$size / 1024, 1)
  ),
  row.names = FALSE
)

# 记录 R 和所用包的版本，方便别人复现
writeLines(
  capture.output(sessionInfo()),
  "data/processed/session_info.txt"
)

cat("\nAll three figure files are present and non-empty.\n")