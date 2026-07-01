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

  x_limits <- c(-5, 5)
  x_breaks <- seq(-5, 5, by = 1)

  # =====================================
  # TOP PANEL (PERSON HISTOGRAM)
  # =====================================

  plot_top <- ggplot(df_person, aes(x = Theta)) +
    
    geom_histogram(
      fill = "grey60", 
      colour = "black", 
      linewidth = 0.4, 
      bins = 25,
      na.rm = TRUE
    ) +
    
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(xlim = x_limits) +
    
    labs(y = "Frequency") +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      
      # PERUBAHAN DI SINI:
      axis.line.x = element_line(colour = "black"),  # Menampilkan garis horizontal
      axis.ticks.x = element_line(colour = "black"), # Menampilkan tanda garis kecil (ticks)
      axis.text.x = element_blank(),                 # TETAP menyembunyikan angka/teks
      axis.title.x = element_blank(),
      
      axis.title.y = element_text(face = "bold", colour = "black"),
      axis.text.y = element_text(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
    )

  # =====================================
  # BOTTOM PANEL (THRESHOLD)
  # =====================================

  plot_bottom <- ggplot(df_thresh_long, aes(y = Item)) +
    
    geom_vline(xintercept = mean_theta, colour = "black", linewidth = 0.6, linetype = "dashed") +
    geom_vline(xintercept = mean_item, colour = "black", linewidth = 0.6, linetype = "dotted") +
    
    geom_line(aes(x = Value, group = Item), colour = "grey50", linewidth = 0.5) +
    
    geom_label(
      aes(x = Value, label = Step_Num), 
      size = 3.2, 
      fontface = "bold", 
      label.size = 0.2, 
      label.padding = unit(0.2, "lines"), 
      fill = "white", 
      colour = "black"
    ) +
    
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(xlim = x_limits) +
    
    labs(
      x = "Logit Scale (θ / τ)",
      y = "Items"
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(face = "bold", colour = "black", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", colour = "black", margin = margin(r = 10)),
      axis.text = element_text(colour = "black"),
      axis.line = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
    )

  # =====================================
  # COMBINE & RENDER
  # =====================================

  final_map <- plot_top / plot_bottom +
    plot_layout(heights = c(1, 3)) +
    plot_annotation(
      # title = "Person-Item Threshold Map",
      subtitle = paste0(
        # "IPOI = ", round(overlap_index, 3), " (", targeting_quality, " Targeting); Mean Gap = ", round(mean_gap, 2), " logits"
      ),
      caption = paste(
        # "Dashed line = Mean Person Ability", "|",
        # "Dotted line = Mean Global Item Difficulty\n",
        # "Numbers (1, 2, 3...) represent the threshold steps."
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, colour = "black"),
        plot.subtitle = element_text(size = 11, colour = "grey20"),
        plot.caption = element_text(size = 10, colour = "grey30", hjust = 0.5, margin = margin(t = 15)),
        plot.background = element_rect(fill = "white", colour = NA),
      )
    )

  return(final_map)
}