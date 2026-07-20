
library(dplyr)
library(flextable)
library(officer)


data_2021 <- read.csv("D:/GBD_data/risk_2021.csv", stringsAsFactors = FALSE)
data_2040 <- read.csv("D:/GBD_data/risk_2040.csv", stringsAsFactors = FALSE)


names(data_2021)


merged_data <- data_2021 %>%
  inner_join(data_2040, by = "location_id", suffix = c("_2021", "_2040"))


h_number_cols <- grep("h_number", colnames(merged_data), value = TRUE)
if (length(h_number_cols) >= 2) {
  flow_data <- merged_data[, c("location_id", h_number_cols[1], h_number_cols[2])]
  colnames(flow_data) <- c("location_id", "h_number_2021", "h_number_2040")
  flow_data$h_number_2021 <- as.numeric(flow_data$h_number_2021)
  flow_data$h_number_2040 <- as.numeric(flow_data$h_number_2040)
  flow_data <- flow_data[!is.na(flow_data$h_number_2021) & !is.na(flow_data$h_number_2040), ]
} else {
  stop("未找到足够的h_number列")
}


frequency_2021 <- flow_data %>%
  group_by(h_number_2021) %>%
  summarise(frequency = n(), .groups = "drop")


flow_data <- flow_data %>%
  mutate(
    change_direction = case_when(
      h_number_2040 < h_number_2021 ~ "Decreased",
      h_number_2040 == h_number_2021 ~ "Unchanged", 
      h_number_2040 > h_number_2021 ~ "Increased"
    ),
    change_magnitude = h_number_2040 - h_number_2021
  )


transition_summary <- flow_data %>%
  group_by(h_number_2021, change_direction) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(h_number_2021) %>%
  mutate(
    total_countries = sum(count),
    percentage = round(count / total_countries * 100, 1)
  ) %>%
  ungroup()


complete_h_numbers <- expand.grid(
  h_number_2021 = 1:16,
  change_direction = c("Decreased", "Unchanged", "Increased")
)


full_transition_summary <- complete_h_numbers %>%
  left_join(transition_summary, by = c("h_number_2021", "change_direction")) %>%
  mutate(
    count = ifelse(is.na(count), 0, count),
    percentage = ifelse(is.na(percentage), 0, percentage),
    total_countries = ifelse(is.na(total_countries), 0, total_countries)
  )


actual_totals <- flow_data %>%
  group_by(h_number_2021) %>%
  summarise(actual_total = n(), .groups = "drop")

full_transition_summary <- full_transition_summary %>%
  left_join(actual_totals, by = "h_number_2021") %>%
  mutate(
    actual_total = ifelse(is.na(actual_total), 0, actual_total),
    percentage = ifelse(actual_total > 0, round(count / actual_total * 100, 1), 0)
  )


wide_summary <- full_transition_summary %>%
  select(h_number_2021, change_direction, count, percentage, actual_total) %>%
  tidyr::pivot_wider(
    names_from = change_direction,
    values_from = c(count, percentage),
    names_sep = "_"
  ) %>%
  arrange(h_number_2021)



h_number_stats <- wide_summary %>%
  mutate(
    total_info = paste0(actual_total),
    decreased_info = ifelse(actual_total > 0, 
                            paste0(count_Decreased, " (", percentage_Decreased, "%)"), 
                            "0 (0%)"),
    unchanged_info = ifelse(actual_total > 0, 
                            paste0(count_Unchanged, " (", percentage_Unchanged, "%)"), 
                            "0 (0%)"),
    increased_info = ifelse(actual_total > 0, 
                            paste0(count_Increased, " (", percentage_Increased, "%)"), 
                            "0 (0%)")
  ) %>%
  select(h_number_2021, total_info, decreased_info, unchanged_info, increased_info)


complete_frequency_2021 <- data.frame(h_number_2021 = 1:16) %>%
  left_join(frequency_2021, by = "h_number_2021") %>%
  mutate(frequency = ifelse(is.na(frequency), 0, frequency))


transposed_data <- data.frame(
  Category = c("H_Number (2021)", "Frequency in 2021", "Total Countries", "Decreased", "Unchanged", "Increased"),
  stringsAsFactors = FALSE
)


for(i in 1:16) {
  col_name <- paste0("H", i)
  
  
  freq_2021 <- complete_frequency_2021[complete_frequency_2021$h_number_2021 == i, "frequency"]
  
  if(i %in% h_number_stats$h_number_2021) {
    row_data <- h_number_stats[h_number_stats$h_number_2021 == i, ]
    transposed_data[[col_name]] <- c(
      as.character(i),
      as.character(freq_2021),
      row_data$total_info,
      row_data$decreased_info,
      row_data$unchanged_info,
      row_data$increased_info
    )
  } else {
    transposed_data[[col_name]] <- c(
      as.character(i),
      as.character(freq_2021),
      "0",
      "0 (0%)",
      "0 (0%)",
      "0 (0%)"
    )
  }
}

final_table <- transposed_data


cat("=== H_Number Transition Analysis (2021 → 2040) ===\n\n")
print(final_table)



ft <- flextable(final_table)

windows()



ft <- ft %>%
  
  #font(fontname = "serif", part = "all") %>%
  fontsize(size = 9, part = "all") %>%  # 
  
  
  bg(bg = "#4472C4", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  
  
  bg(j = 1, bg = "#D9E2F3") %>%
  bold(j = 1) %>%
  
  
  bg(i = 2, bg = "#E7E6E6") %>%
  bold(i = 2) %>%
  
  
  border_outer(part = "all", border = fp_border(color = "black", width = 1)) %>%
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  
  
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:17, align = "center", part = "body") %>%
  
  
  width(j = 1, width = 1.2) %>%
  width(j = 2:17, width = 0.8) %>%
  
  
  height_all(height = 0.3)


doc <- read_docx()


doc <- doc %>%
  body_set_default_section(
    prop_section(
      page_size = page_size(orient = "landscape"),
      page_margins = page_mar(bottom = 0.5, top = 0.5, right = 0.5, left = 0.5)
    )
  )


doc <- doc %>%
  body_add_par("Table 2. H_Number Transition Patterns from 2021 to 2040", style = "heading 1") %>%
  body_add_par("") %>%
  body_add_flextable(ft) %>%
  body_add_par("") %>%
  body_add_par("Note: This table shows the transition patterns of h_number values between 2021 and 2040.") %>%
  body_add_par("'Frequency in 2021' shows the total number of countries with each h_number value in 2021.") %>%
  body_add_par("Numbers in parentheses represent percentages within each 2021 h_number category.") %>%
  body_add_par("'Decreased' indicates h_number value decreased from 2021 to 2040.") %>%
  body_add_par("'Unchanged' indicates h_number value remained the same.") %>%
  body_add_par("'Increased' indicates h_number value increased from 2021 to 2040.") %>%
  body_add_par("") %>%
  body_add_par(paste("Total countries analyzed:", nrow(flow_data))) %>%
  body_add_par(paste("Table generated on:", Sys.time()))


doc_path <- "D:/GBD_data/h_number_transition_analysis_2021_2040.docx"
print(doc, target = doc_path)


csv_path <- "D:/GBD_data/h_number_transition_analysis_2021_2040.csv"
write.csv(final_table, csv_path, row.names = FALSE, fileEncoding = "UTF-8")


cat("\n=== Summary Statistics ===\n")
cat("Total countries analyzed:", nrow(flow_data), "\n")


cat("\n2021 H_Number frequency distribution:\n")
for(i in 1:16) {
  freq <- complete_frequency_2021[complete_frequency_2021$h_number_2021 == i, "frequency"]
  if(freq > 0) {
    cat("H_Number", i, ":", freq, "countries\n")
  }
}


overall_changes <- flow_data %>%
  group_by(change_direction) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round(count / nrow(flow_data) * 100, 1))

cat("\nOverall transition patterns:\n")
for(i in 1:nrow(overall_changes)) {
  cat(overall_changes$change_direction[i], ":", overall_changes$count[i], 
      "countries (", overall_changes$percentage[i], "%)\n")
}


most_stable <- final_table[which.max(as.numeric(gsub(".*\\((.*)%\\).*", "\\1", final_table$Unchanged))), ]
cat("\nMost stable h_number value:", most_stable$Category, 
    "with", most_stable$Unchanged, "remaining unchanged\n")


h_numbers_with_data <- which(complete_frequency_2021$frequency > 0)
cat("H_number values with data in 2021:", paste(h_numbers_with_data, collapse = ", "), "\n")

cat("\nFiles saved:\n")
cat("Word document:", doc_path, "\n")
cat("CSV data:", csv_path, "\n")


























#######################################################################################################################################################################







if ("location_name_2021" %in% colnames(merged_data)) {
  location_name_col <- "location_name_2021"
} else if ("location_name_2040" %in% colnames(merged_data)) {
  location_name_col <- "location_name_2040"
} else if ("location_name" %in% colnames(merged_data)) {
  
  
  cat("Warning: location_name column not found in merged data. Trying to get from original data...\n")
  
  
  if ("location_name" %in% colnames(data_2021)) {
    flow_data_with_names <- flow_data %>%
      left_join(data_2021[, c("location_id", "location_name")], by = "location_id")
  } else {
    stop("location_name column not found in the data")
  }
} else {
  
  flow_data_with_names <- flow_data %>%
    left_join(merged_data[, c("location_id", location_name_col)], by = "location_id") %>%
    rename(location_name = !!location_name_col)
}


if (!exists("flow_data_with_names")) {
  if ("location_name" %in% colnames(data_2021)) {
    flow_data_with_names <- flow_data %>%
      left_join(data_2021[, c("location_id", "location_name")], by = "location_id")
  } else {
    stop("Cannot find location_name column in the data")
  }
}


cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("DETAILED COUNTRY ANALYSIS BY H_NUMBER CHANGE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")


increased_countries <- flow_data_with_names %>%
  filter(change_direction == "Increased") %>%
  arrange(h_number_2021, h_number_2040)

cat("📈 COUNTRIES WITH INCREASED H_NUMBER (", nrow(increased_countries), " countries):\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

if (nrow(increased_countries) > 0) {
  for (h_num in sort(unique(increased_countries$h_number_2021))) {
    countries_in_h <- increased_countries %>% filter(h_number_2021 == h_num)
    cat(sprintf("From H_Number %d (%d countries):\n", h_num, nrow(countries_in_h)))
    
    for (i in 1:nrow(countries_in_h)) {
      country_info <- countries_in_h[i, ]
      cat(sprintf("  • %s: %d → %d (change: +%d)\n", 
                  country_info$location_name,
                  country_info$h_number_2021,
                  country_info$h_number_2040,
                  country_info$change_magnitude))
    }
    cat("\n")
  }
} else {
  cat("  No countries showed increased h_number values.\n\n")
}


decreased_countries <- flow_data_with_names %>%
  filter(change_direction == "Decreased") %>%
  arrange(h_number_2021, h_number_2040)

cat("📉 COUNTRIES WITH DECREASED H_NUMBER (", nrow(decreased_countries), " countries):\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

if (nrow(decreased_countries) > 0) {
  for (h_num in sort(unique(decreased_countries$h_number_2021))) {
    countries_in_h <- decreased_countries %>% filter(h_number_2021 == h_num)
    cat(sprintf("From H_Number %d (%d countries):\n", h_num, nrow(countries_in_h)))
    
    for (i in 1:nrow(countries_in_h)) {
      country_info <- countries_in_h[i, ]
      cat(sprintf("  • %s: %d → %d (change: %d)\n", 
                  country_info$location_name,
                  country_info$h_number_2021,
                  country_info$h_number_2040,
                  country_info$change_magnitude))
    }
    cat("\n")
  }
} else {
  cat("  No countries showed decreased h_number values.\n\n")
}


unchanged_countries <- flow_data_with_names %>%
  filter(change_direction == "Unchanged") %>%
  arrange(h_number_2021)

cat("➡️ COUNTRIES WITH UNCHANGED H_NUMBER (", nrow(unchanged_countries), " countries):\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

if (nrow(unchanged_countries) > 0) {
  for (h_num in sort(unique(unchanged_countries$h_number_2021))) {
    countries_in_h <- unchanged_countries %>% filter(h_number_2021 == h_num)
    cat(sprintf("H_Number %d (%d countries):\n", h_num, nrow(countries_in_h)))
    
    
    country_names <- sort(countries_in_h$location_name)
    
    
    for (i in seq(1, length(country_names), 3)) {
      end_idx <- min(i + 2, length(country_names))
      line_countries <- country_names[i:end_idx]
      cat(sprintf("  • %s\n", paste(line_countries, collapse = ", ")))
    }
    cat("\n")
  }
} else {
  cat("  No countries maintained the same h_number values.\n\n")
}


detailed_analysis <- flow_data_with_names %>%
  arrange(change_direction, h_number_2021, location_name) %>%
  select(location_name, h_number_2021, h_number_2040, change_direction, change_magnitude)


detailed_csv_path <- "D:/GBD_data/h_number_country_detailed_analysis_2021_2040.csv"
write.csv(detailed_analysis, detailed_csv_path, row.names = FALSE, fileEncoding = "UTF-8")


summary_by_direction <- flow_data_with_names %>%
  group_by(change_direction, h_number_2021) %>%
  summarise(
    country_count = n(),
    countries = paste(sort(location_name), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(change_direction, h_number_2021)

summary_csv_path <- "D:/GBD_data/h_number_summary_by_direction_2021_2040.csv"
write.csv(summary_by_direction, summary_csv_path, row.names = FALSE, fileEncoding = "UTF-8")


cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY STATISTICS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat(sprintf("Total countries analyzed: %d\n\n", nrow(flow_data_with_names)))


direction_summary <- flow_data_with_names %>%
  group_by(change_direction) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round(count / nrow(flow_data_with_names) * 100, 1))

for (i in 1:nrow(direction_summary)) {
  cat(sprintf("%s: %d countries (%.1f%%)\n", 
              direction_summary$change_direction[i],
              direction_summary$count[i],
              direction_summary$percentage[i]))
}


max_increase <- flow_data_with_names %>% 
  filter(change_direction == "Increased") %>%
  filter(change_magnitude == max(change_magnitude, na.rm = TRUE))

max_decrease <- flow_data_with_names %>% 
  filter(change_direction == "Decreased") %>%
  filter(change_magnitude == min(change_magnitude, na.rm = TRUE))

if (nrow(max_increase) > 0) {
  cat(sprintf("\nLargest increase: %s (%d → %d, +%d)\n", 
              max_increase$location_name[1],
              max_increase$h_number_2021[1],
              max_increase$h_number_2040[1],
              max_increase$change_magnitude[1]))
}

if (nrow(max_decrease) > 0) {
  cat(sprintf("Largest decrease: %s (%d → %d, %d)\n", 
              max_decrease$location_name[1],
              max_decrease$h_number_2021[1],
              max_decrease$h_number_2040[1],
              max_decrease$change_magnitude[1]))
}

cat("\nFiles saved:\n")
cat("Detailed country analysis:", detailed_csv_path, "\n")
cat("Summary by direction:", summary_csv_path, "\n")

cat(paste(rep("=", 80), collapse = ""), "\n")














########################################################################







increased_countries_detailed <- flow_data_with_names %>%
  filter(change_direction == "Increased") %>%
  arrange(change_magnitude, h_number_2021, location_name)


transition_magnitude_summary <- increased_countries_detailed %>%
  group_by(change_magnitude) %>%
  summarise(
    country_count = n(),
    countries = paste(sort(location_name), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(change_magnitude)

cat("\n", paste(rep("=", 90), collapse = ""), "\n")
cat("DETAILED ANALYSIS: INCREASED COUNTRIES BY TRANSITION MAGNITUDE\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")


cat("📊 OVERVIEW OF TRANSITION MAGNITUDES:\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
for(i in 1:nrow(transition_magnitude_summary)) {
  magnitude <- transition_magnitude_summary$change_magnitude[i]
  count <- transition_magnitude_summary$country_count[i]
  percentage <- round(count / nrow(increased_countries_detailed) * 100, 1)
  cat(sprintf("Jump by +%d: %d countries (%.1f%%)\n", magnitude, count, percentage))
}
cat("\n")


for(magnitude in sort(unique(increased_countries_detailed$change_magnitude))) {
  countries_in_magnitude <- increased_countries_detailed %>%
    filter(change_magnitude == magnitude) %>%
    arrange(h_number_2021, location_name)
  
  cat(sprintf("🔥 COUNTRIES WITH +%d H_NUMBER INCREASE (%d countries):\n", 
              magnitude, nrow(countries_in_magnitude)))
  cat(paste(rep("-", 70), collapse = ""), "\n")
  
  
  for(start_h in sort(unique(countries_in_magnitude$h_number_2021))) {
    countries_from_h <- countries_in_magnitude %>%
      filter(h_number_2021 == start_h)
    
    cat(sprintf("\n  From H_Number %d to H_Number %d (%d countries):\n", 
                start_h, start_h + magnitude, nrow(countries_from_h)))
    
    
    country_names <- countries_from_h$location_name
    for(i in seq(1, length(country_names), 3)) {
      end_idx <- min(i + 2, length(country_names))
      line_countries <- country_names[i:end_idx]
      cat(sprintf("    • %s\n", paste(line_countries, collapse = ", ")))
    }
  }
  cat("\n")
}


ethiopia_data <- increased_countries_detailed %>%
  filter(grepl("Ethiopia", location_name, ignore.case = TRUE))

if(nrow(ethiopia_data) > 0) {
  cat("🇪🇹 ETHIOPIA EXAMPLE:\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")
  cat(sprintf("Ethiopia: H_Number %d → H_Number %d (Jump: +%d)\n", 
              ethiopia_data$h_number_2021[1], 
              ethiopia_data$h_number_2040[1], 
              ethiopia_data$change_magnitude[1]))
  cat("This means Ethiopia will face", ethiopia_data$change_magnitude[1], 
      "additional risk factors above P50 threshold by 2040.\n\n")
}


detailed_transition_analysis <- increased_countries_detailed %>%
  select(location_name, h_number_2021, h_number_2040, change_magnitude) %>%
  arrange(change_magnitude, h_number_2021, location_name)


detailed_transition_analysis <- detailed_transition_analysis %>%
  mutate(
    transition_level = case_when(
      change_magnitude == 1 ~ "Minimal Increase (+1)",
      change_magnitude == 2 ~ "Moderate Increase (+2)", 
      change_magnitude == 3 ~ "Substantial Increase (+3)",
      change_magnitude == 4 ~ "Major Increase (+4)",
      change_magnitude == 5 ~ "Severe Increase (+5)",
      change_magnitude >= 6 ~ "Extreme Increase (+6 or more)",
      TRUE ~ paste0("Increase (+", change_magnitude, ")")
    )
  )


transition_level_summary <- detailed_transition_analysis %>%
  group_by(transition_level, change_magnitude) %>%
  summarise(
    country_count = n(),
    avg_start_h = round(mean(h_number_2021), 1),
    avg_end_h = round(mean(h_number_2040), 1),
    countries = paste(sort(location_name), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(change_magnitude)

cat("📈 TRANSITION LEVEL ANALYSIS:\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
for(i in 1:nrow(transition_level_summary)) {
  level_info <- transition_level_summary[i, ]
  cat(sprintf("\n%s:\n", level_info$transition_level))
  cat(sprintf("  • Number of countries: %d\n", level_info$country_count))
  cat(sprintf("  • Average starting H_Number: %.1f\n", level_info$avg_start_h))
  cat(sprintf("  • Average ending H_Number: %.1f\n", level_info$avg_end_h))
  
  
  countries_list <- strsplit(level_info$countries, "; ")[[1]]
  if(length(countries_list) <= 5) {
    cat(sprintf("  • Countries: %s\n", paste(countries_list, collapse = ", ")))
  } else {
    cat(sprintf("  • Examples: %s, ... (and %d more)\n", 
                paste(countries_list[1:5], collapse = ", "), 
                length(countries_list) - 5))
  }
}


max_transition <- increased_countries_detailed %>%
  filter(change_magnitude == max(change_magnitude))

cat(sprintf("\n🚨 COUNTRIES WITH MAXIMUM TRANSITION (+%d):\n", 
            max(increased_countries_detailed$change_magnitude)))
cat(paste(rep("-", 50), collapse = ""), "\n")
for(i in 1:nrow(max_transition)) {
  country_info <- max_transition[i, ]
  cat(sprintf("• %s: %d → %d\n", 
              country_info$location_name,
              country_info$h_number_2021,
              country_info$h_number_2040))
}


starting_point_analysis <- increased_countries_detailed %>%
  group_by(h_number_2021) %>%
  summarise(
    country_count = n(),
    avg_transition = round(mean(change_magnitude), 1),
    max_transition = max(change_magnitude),
    min_transition = min(change_magnitude),
    .groups = "drop"
  ) %>%
  arrange(h_number_2021)

cat("\n📍 TRANSITION PATTERNS BY STARTING H_NUMBER:\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat(sprintf("%-12s %-10s %-15s %-12s %-12s\n", 
            "Start H_Num", "Countries", "Avg Transition", "Min Jump", "Max Jump"))
cat(paste(rep("-", 65), collapse = ""), "\n")

for(i in 1:nrow(starting_point_analysis)) {
  row_data <- starting_point_analysis[i, ]
  cat(sprintf("%-12d %-10d %-15.1f %-12d %-12d\n",
              row_data$h_number_2021,
              row_data$country_count,
              row_data$avg_transition,
              row_data$min_transition,
              row_data$max_transition))
}


transition_analysis_path <- "D:/GBD_data/h_number_transition_magnitude_analysis_2021_2040.csv"
write.csv(detailed_transition_analysis, transition_analysis_path, row.names = FALSE, fileEncoding = "UTF-8")

transition_summary_path <- "D:/GBD_data/h_number_transition_level_summary_2021_2040.csv"
write.csv(transition_level_summary, transition_summary_path, row.names = FALSE, fileEncoding = "UTF-8")

starting_point_path <- "D:/GBD_data/h_number_starting_point_analysis_2021_2040.csv"
write.csv(starting_point_analysis, starting_point_path, row.names = FALSE, fileEncoding = "UTF-8")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("FILES SAVED:\n")
cat("• Detailed transition analysis:", transition_analysis_path, "\n")
cat("• Transition level summary:", transition_summary_path, "\n") 
cat("• Starting point analysis:", starting_point_path, "\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
















cat("=== 验证跃迁分层分布特征 ===\n\n")


increased_countries <- flow_data_with_names %>%
  filter(change_direction == "Increased") %>%
  mutate(
    transition_category = case_when(
      change_magnitude == 1 ~ "轻微跃迁(+1)",
      change_magnitude == 2 ~ "中等跃迁(+2)", 
      change_magnitude == 3 ~ "显著跃迁(+3)",
      change_magnitude == 4 ~ "重大跃迁(+4)",
      change_magnitude == 5 ~ "极端跃迁(+5)",
      TRUE ~ paste0("其他跃迁(+", change_magnitude, ")")
    )
  )


total_increased <- nrow(increased_countries)
cat(sprintf("✓ 验证: 风险因素增加的国家总数: %d\n", total_increased))
if(total_increased == 122) {
  cat("✓ 符合描述: 122个国家\n\n")
} else {
  cat(sprintf("✗ 不符合描述: 实际为%d个国家，描述为122个\n\n", total_increased))
}


transition_stats <- increased_countries %>%
  group_by(change_magnitude, transition_category) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = round(count / total_increased * 100, 1)
  ) %>%
  arrange(change_magnitude)

cat("=== 各跃迁等级统计验证 ===\n")
for(i in 1:nrow(transition_stats)) {
  magnitude <- transition_stats$change_magnitude[i]
  category <- transition_stats$transition_category[i]
  count <- transition_stats$count[i] 
  percentage <- transition_stats$percentage[i]
  
  cat(sprintf("%s: %d国 (%.1f%%)\n", category, count, percentage))
}
cat("\n")


cat("=== 具体数据验证 ===\n")


mild_data <- transition_stats[transition_stats$change_magnitude == 1, ]
cat(sprintf("轻微跃迁 - 描述: 44.3%% (54国), 实际: %.1f%% (%d国)\n", 
            mild_data$percentage, mild_data$count))


moderate_data <- transition_stats[transition_stats$change_magnitude == 2, ]
cat(sprintf("中等跃迁 - 描述: 28.7%% (35国), 实际: %.1f%% (%d国)\n", 
            moderate_data$percentage, moderate_data$count))


substantial_data <- transition_stats[transition_stats$change_magnitude == 3, ]  
cat(sprintf("显著跃迁 - 描述: 18.0%% (22国), 实际: %.1f%% (%d国)\n", 
            substantial_data$percentage, substantial_data$count))


major_data <- transition_stats[transition_stats$change_magnitude == 4, ]
cat(sprintf("重大跃迁 - 描述: 7.4%% (9国), 实际: %.1f%% (%d国)\n", 
            major_data$percentage, major_data$count))


extreme_data <- transition_stats[transition_stats$change_magnitude == 5, ]
cat(sprintf("极端跃迁 - 描述: 1.6%% (2国), 实际: %.1f%% (%d国)\n", 
            extreme_data$percentage, extreme_data$count))

cat("\n")


cat("=== 验证具体国家分类 ===\n")


mild_countries <- increased_countries %>% filter(change_magnitude == 1)
mentioned_mild <- c("Australia", "France", "Denmark", "Viet Nam", "Philippines", "Kenya")

cat("轻微跃迁国家验证:\n")
for(country in mentioned_mild) {
  is_present <- any(grepl(country, mild_countries$location_name, ignore.case = TRUE))
  status <- ifelse(is_present, "✓", "✗")
  cat(sprintf("  %s %s\n", status, country))
}


moderate_countries <- increased_countries %>% filter(change_magnitude == 2)  
mentioned_moderate <- c("Germany", "United States", "Sweden", "China", "Ethiopia", "Morocco")

cat("\n中等跃迁国家验证:\n")
for(country in mentioned_moderate) {
  is_present <- any(grepl(country, moderate_countries$location_name, ignore.case = TRUE))
  status <- ifelse(is_present, "✓", "✗")
  cat(sprintf("  %s %s\n", status, country))
}


substantial_countries <- increased_countries %>% filter(change_magnitude == 3)
mentioned_substantial <- c("Bolivia", "Ecuador", "Honduras", "Netherlands", "Iceland")

cat("\n显著跃迁国家验证:\n") 
for(country in mentioned_substantial) {
  is_present <- any(grepl(country, substantial_countries$location_name, ignore.case = TRUE))
  status <- ifelse(is_present, "✓", "✗")
  cat(sprintf("  %s %s\n", status, country))
}


major_countries <- increased_countries %>% filter(change_magnitude == 4)
mentioned_major <- c("China", "Botswana", "Ghana", "Zimbabwe")

cat("\n重大跃迁国家验证:\n")
for(country in mentioned_major) {
  is_present <- any(grepl(country, major_countries$location_name, ignore.case = TRUE))
  status <- ifelse(is_present, "✓", "✗")
  cat(sprintf("  %s %s\n", status, country))
}


extreme_countries <- increased_countries %>% filter(change_magnitude == 5)
mentioned_extreme <- c("Bangladesh", "Saint Vincent and the Grenadines")

cat("\n极端跃迁国家验证:\n")
for(country in mentioned_extreme) {
  is_present <- any(grepl(country, extreme_countries$location_name, ignore.case = TRUE))
  status <- ifelse(is_present, "✓", "✗")
  cat(sprintf("  %s %s\n", status, country))
}


cat("\n\n=== 地理分布验证 ===\n")


get_region <- function(country_name) {
  
  asia <- c("China", "Bangladesh", "Malaysia", "Viet Nam", "Philippines", "Thailand", 
            "Indonesia", "India", "Japan", "South Korea", "Singapore")
  
  
  europe <- c("Germany", "France", "Netherlands", "Iceland", "Sweden", "Denmark",
              "United Kingdom", "Italy", "Spain", "Norway", "Finland")
  
  
  africa <- c("Ethiopia", "Morocco", "Kenya", "Botswana", "Ghana", "Zimbabwe",
              "Nigeria", "South Africa", "Egypt", "Algeria")
  
  
  latin_america <- c("Bolivia", "Ecuador", "Honduras", "Brazil", "Mexico", "Argentina",
                     "Colombia", "Peru", "Chile", "Venezuela")
  
  
  north_america <- c("United States", "Canada")
  
  
  oceania <- c("Australia", "New Zealand")
  
  if(any(sapply(asia, function(x) grepl(x, country_name, ignore.case = TRUE)))) return("亚洲")
  if(any(sapply(europe, function(x) grepl(x, country_name, ignore.case = TRUE)))) return("欧洲") 
  if(any(sapply(africa, function(x) grepl(x, country_name, ignore.case = TRUE)))) return("非洲")
  if(any(sapply(latin_america, function(x) grepl(x, country_name, ignore.case = TRUE)))) return("拉丁美洲")
  if(any(sapply(north_america, function(x) grepl(x, country_name, ignore.case = TRUE)))) return("北美洲")
  if(any(sapply(oceania, function(x) grepl(x, country_name, ignore.case = TRUE)))) return("大洋洲")
  return("其他")
}


increased_countries_with_region <- increased_countries %>%
  mutate(region = sapply(location_name, get_region))


regional_analysis <- increased_countries_with_region %>%
  group_by(region, change_magnitude) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = change_magnitude, 
                     values_from = count, 
                     values_fill = 0,
                     names_prefix = "跃迁+") %>%
  arrange(region)

print(regional_analysis)

cat("\n=== 金字塔型分布验证 ===\n")
cat("跃迁等级从+1到+5的国家数量应呈递减趋势:\n")
for(i in 1:5) {
  count_row <- transition_stats[transition_stats$change_magnitude == i, ]
  if(nrow(count_row) > 0) {
    actual_count <- count_row$count[1]  
    cat(sprintf("+%d: %d国\n", i, actual_count))
  }
}


pyramid_counts <- c()
for(i in 1:5) {
  count_row <- transition_stats[transition_stats$change_magnitude == i, ]
  if(nrow(count_row) > 0) {
    pyramid_counts <- c(pyramid_counts, count_row$count[1])
  } else {
    pyramid_counts <- c(pyramid_counts, 0)
  }
}

is_pyramid <- all(diff(pyramid_counts) <= 0)
cat(sprintf("\n金字塔型分布验证: %s\n", ifelse(is_pyramid, "✓ 符合", "✗ 不符合")))


cat("完整序列:", paste(pyramid_counts, collapse = " > "), "\n")








china_data <- flow_data_with_names %>%
  filter(grepl("China", location_name, ignore.case = TRUE))

print(china_data[, c("location_name", "h_number_2021", "h_number_2040", "change_magnitude")])




malaysia_data <- flow_data_with_names %>%
  filter(grepl("Malaysia", location_name, ignore.case = TRUE))
print(malaysia_data[, c("location_name", "h_number_2021", "h_number_2040", "change_magnitude")])