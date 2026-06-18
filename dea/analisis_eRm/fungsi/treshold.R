plot_threshold_map <- function(model_object) {

  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(dplyr)
  library(tidyr) 

  # =====================================
  # PERSON ABILITY 
  # =====================================

  pp <- person.parameter(model_object)
  theta <- as.numeric(pp$theta.table[, "Person Parameter"])

  df_person <- data.frame(Theta = theta)

  mean_theta <- mean(theta, na.rm = TRUE)
  sd_theta <- sd(theta, na.rm = TRUE)

  df_person$Level <- cut(
    df_person$Theta,
    breaks = c(-Inf, mean_theta - sd_theta, mean_theta + sd_theta, Inf),
    labels = c("Low", "Moderate", "High")
  )

  df_person$Level <- factor(
    df_person$Level,
    levels = c("High", "Moderate", "Low")
  )

  # =====================================
  # ITEM THRESHOLD 
  # =====================================

  thresh_obj <- thresholds(model_object)
  thresh_matrix <- thresh_obj$threshtable[[1]]

  df_item_global <- data.frame(
    Item = rownames(thresh_matrix),
    Location = thresh_matrix[, "Location"]
  ) %>% arrange(Location)

  mean_item <- mean(df_item_global$Location, na.rm = TRUE)

  df_thresh <- as.data.frame(thresh_matrix)
  df_thresh$Item <- rownames(df_thresh)
  
  df_thresh$Item <- factor(df_thresh$Item, levels = df_item_global$Item)

  df_thresh_long <- df_thresh %>%
    select(-Location) %>%
    pivot_longer(
      cols = starts_with("Threshold"),
      names_to = "Threshold_Level",
      values_to = "Value"
    ) %>%
    filter(!is.na(Value)) %>%
    mutate(Step_Num = gsub("Threshold ", "", Threshold_Level))

  # =====================================
  # TARGETING & LIMITS
  # =====================================

  range_all <- max(c(theta, df_item_global$Location), na.rm = TRUE) - 
               min(c(theta, df_item_global$Location), na.rm = TRUE)

  overlap_index <- 1 - abs(mean_theta - mean_item) / range_all
  mean_gap <- abs(mean_theta - mean_item)

  targeting_quality <- dplyr::case_when(
    overlap_index >= 0.90 ~ "Excellent",
    overlap_index >= 0.80 ~ "Good",
    overlap_index >= 0.70 ~ "Moderate",
    TRUE ~ "Poor"
  )

  x_limits <- range(c(theta, df_thresh_long$Value), na.rm = TRUE)
  x_limits <- c(x_limits[1] - 0.5, x_limits[2] + 0.5)

  # =====================================
  # TOP PANEL (PERSON) - KANVAS PUTIH BERSIH
  # =====================================

  plot_top <- ggplot(df_person, aes(x = Theta)) +
    geom_density(aes(fill = Level), alpha = 0.4, linewidth = 0.4) +
    geom_vline(xintercept = mean_theta, colour = "#2E86C1", linewidth = 0.5, linetype = "dashed") +
    geom_vline(xintercept = mean_item, colour = "#C0392B", linewidth = 0.5, linetype = "dashed") +
    scale_fill_manual(
      name = "Person Ability\nLevel",
      values = c("High" = "#E74C3C", "Moderate" = "#F1C40F", "Low" = "#3498DB")
    ) +
    scale_x_continuous(limits = x_limits) +
    labs(y = "Density") +
    theme_classic(base_size = 12) + # Diganti ke classic agar bersih
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold"),
      axis.line.x = element_blank(), # Menghilangkan garis sumbu X di panel atas
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )

  # =====================================
  # BOTTOM PANEL (THRESHOLD) - KANVAS PUTIH BERSIH
  # =====================================

  plot_bottom <- ggplot(df_thresh_long, aes(y = Item)) +
    geom_vline(xintercept = mean_theta, colour = "#2E86C1", linewidth = 0.55, linetype = "dashed") +
    geom_vline(xintercept = mean_item, colour = "#C0392B", linewidth = 0.55, linetype = "dashed") +
    
    geom_line(aes(x = Value, group = Item), colour = "grey60", linewidth = 0.6) +
    
    geom_label(
      aes(x = Value, label = Step_Num), 
      size = 3.5, 
      fontface = "bold", 
      label.size = 0, 
      label.padding = unit(0.15, "lines"), 
      fill = "white", 
      colour = "black"
    ) +
    
    scale_x_continuous(limits = x_limits) +
    labs(
      x = "Logit Scale (θ / τ)",
      y = "Items"
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      legend.position = "none",
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )

  # =====================================
  # COMBINE & RENDER
  # =====================================

  final_map <- plot_top / plot_bottom +
    plot_layout(heights = c(1, 3)) +
    plot_annotation(
      title = "Detailed Person-Item Threshold Map",
      subtitle = paste0(
        "IPOI = ", round(overlap_index, 3), " (", targeting_quality, " Targeting); Mean Gap = ", round(mean_gap, 2), " logits"
      ),
      caption = paste(
        "Blue dashed line = Mean Person Ability", "|",
        "Red dashed line = Mean Global Item Difficulty",
        "Numbers (1, 2, 3, 4) represent the threshold steps."
      ),
      theme = theme(
        plot.background = element_rect(fill = "white", colour = NA)
      )
    )

  return(final_map)
}