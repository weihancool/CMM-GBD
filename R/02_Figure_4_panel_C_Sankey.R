rm(list = ls())

library(dplyr)
library(plotly)
library(htmlwidgets)

data_2021 <- read.csv(
  "D:/GBD_data/risk_2021.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

data_2040 <- read.csv(
  "D:/GBD_data/risk_2040.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c("location_id", "h_number")

missing_2021 <- setdiff(required_columns, names(data_2021))
missing_2040 <- setdiff(required_columns, names(data_2040))

if (length(missing_2021) > 0) {
  stop(
    paste0(
      "2021年数据缺少变量：",
      paste(missing_2021, collapse = ", ")
    )
  )
}

if (length(missing_2040) > 0) {
  stop(
    paste0(
      "2040年数据缺少变量：",
      paste(missing_2040, collapse = ", ")
    )
  )
}

flow_data <- data_2021 %>%
  select(
    location_id,
    h_number_2021 = h_number
  ) %>%
  inner_join(
    data_2040 %>%
      select(
        location_id,
        h_number_2040 = h_number
      ),
    by = "location_id"
  ) %>%
  mutate(
    h_number_2021 = suppressWarnings(as.numeric(h_number_2021)),
    h_number_2040 = suppressWarnings(as.numeric(h_number_2040))
  )

n_before <- nrow(flow_data)

flow_data <- flow_data %>%
  filter(
    !is.na(h_number_2021),
    !is.na(h_number_2040),
    h_number_2021 %in% 1:16,
    h_number_2040 %in% 1:16
  ) %>%
  mutate(
    h_number_2021 = as.integer(h_number_2021),
    h_number_2040 = as.integer(h_number_2040)
  )

n_after <- nrow(flow_data)

if (n_after < n_before) {
  warning(
    paste0(
      n_before - n_after,
      " 个国家或地区因h_number缺失或超出1–16范围而被排除。"
    )
  )
}

if (nrow(flow_data) == 0) {
  stop("清理后没有可用于绘图的数据。")
}

duplicated_locations <- flow_data %>%
  count(location_id) %>%
  filter(n > 1)

if (nrow(duplicated_locations) > 0) {
  stop(
    paste0(
      "发现 ",
      nrow(duplicated_locations),
      " 个location_id存在重复记录。"
    )
  )
}

flow_summary <- flow_data %>%
  count(
    h_number_2021,
    h_number_2040,
    name = "count"
  ) %>%
  filter(count > 0) %>%
  arrange(h_number_2021, h_number_2040)

all_h_values <- 1:16

node_labels <- c(
  as.character(all_h_values),
  as.character(all_h_values)
)

color_palette <- grDevices::colorRampPalette(
  c(
    "#2C7FB8",
    "#41B6C4",
    "#7FCDBB",
    "#C7E9B4",
    "#A1D76A",
    "#FFFF8C",
    "#FED976",
    "#FEB24C",
    "#FD8D3C",
    "#FC4E2A",
    "#E31A1C",
    "#BD0026"
  )
)(16)

hex_to_rgba <- function(hex_colors, alpha = 1) {
  rgb_matrix <- grDevices::col2rgb(hex_colors)
  
  apply(
    rgb_matrix,
    2,
    function(x) {
      sprintf(
        "rgba(%d,%d,%d,%.2f)",
        x[1],
        x[2],
        x[3],
        alpha
      )
    }
  )
}

node_colors <- c(
  hex_to_rgba(color_palette, alpha = 0.95),
  hex_to_rgba(color_palette, alpha = 0.70)
)

source_indices <- flow_summary$h_number_2021 - 1
target_indices <- flow_summary$h_number_2040 - 1 + 16

link_colors <- hex_to_rgba(
  color_palette[flow_summary$h_number_2021],
  alpha = 0.50
)

link_hover_text <- paste0(
  "2021 level: ",
  flow_summary$h_number_2021,
  "<br>2040 level: ",
  flow_summary$h_number_2040,
  "<br>Countries/territories: ",
  flow_summary$count
)

node_x_positions <- seq(
  from = 0.01,
  to = 0.99,
  length.out = 16
)

node_x <- c(
  node_x_positions,
  node_x_positions
)

node_y <- c(
  rep(0.02, 16),
  rep(0.98, 16)
)

fig <- plot_ly(
  type = "sankey",
  orientation = "v",
  arrangement = "fixed",
  node = list(
    label = node_labels,
    x = node_x,
    y = node_y,
    color = node_colors,
    pad = 8,
    thickness = 14,
    line = list(
      color = "rgba(60,60,60,0.80)",
      width = 0.5
    ),
    hovertemplate = paste0(
      "Number of risk factors above P50: %{label}",
      "<extra></extra>"
    )
  ),
  link = list(
    source = source_indices,
    target = target_indices,
    value = flow_summary$count,
    color = link_colors,
    customdata = link_hover_text,
    hovertemplate = "%{customdata}<extra></extra>"
  )
)

fig <- fig %>%
  layout(
    title = NULL,
    font = list(
      family = "Arial",
      size = 10,
      color = "black"
    ),
    width = 1600,
    height = 650,
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    margin = list(
      l = 35,
      r = 20,
      t = 45,
      b = 35
    ),
    annotations = list(
      list(
        x = -0.015,
        y = 1.08,
        xref = "paper",
        yref = "paper",
        text = "<b>C</b>",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "top",
        font = list(
          family = "Times New Roman",
          size = 24,
          color = "black"
        )
      ),
      list(
        x = 0.50,
        y = 1.055,
        xref = "paper",
        yref = "paper",
        text = "2021",
        showarrow = FALSE,
        font = list(
          family = "Arial",
          size = 12,
          color = "black"
        )
      ),
      list(
        x = 0.50,
        y = -0.055,
        xref = "paper",
        yref = "paper",
        text = "2040",
        showarrow = FALSE,
        font = list(
          family = "Arial",
          size = 12,
          color = "black"
        )
      )
    )
  )

fig

htmlwidgets::saveWidget(
  widget = fig,
  file = "D:/GBD_data/Figure_C_Sankey_2021_2040.html",
  selfcontained = TRUE
)

cat("纳入国家或地区数量：", nrow(flow_data), "\n")
cat("实际跃迁路径数量：", nrow(flow_summary), "\n")
cat("文件已保存为：D:/GBD_data/Figure_C_Sankey_2021_2040.html\n")