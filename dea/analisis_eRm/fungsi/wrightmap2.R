plot_wright_map2 <- function(model_object) {

  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(dplyr)

  # =====================================
  # PERSON ABILITY
  # =====================================

  pp <- person.parameter(model_object)

  theta <- as.numeric(
    pp$theta.table[, "Person Parameter"]
  )

  df_person <- data.frame(
    Theta = theta
  )

  mean_theta <- mean(
    theta,
    na.rm = TRUE
  )

  # =====================================
  # ITEM DIFFICULTY
  # =====================================

  thresh_obj <- thresholds(model_object)

  thresh_matrix <- thresh_obj$threshtable[[1]]

  df_item <- data.frame(
    Item = rownames(thresh_matrix),
    Difficulty = thresh_matrix[, "Location"]
  )

  df_item <- df_item %>%
    arrange(Difficulty)

  mean_item <- mean(
    df_item$Difficulty,
    na.rm = TRUE
  )

  # =====================================
  # TARGETING
  # =====================================

  range_all <-
    max(
      c(theta, df_item$Difficulty),
      na.rm = TRUE
    ) -
    min(
      c(theta, df_item$Difficulty),
      na.rm = TRUE
    )

  overlap_index <-
    1 -
    abs(mean_theta - mean_item) /
    range_all

  mean_gap <- abs(
    mean_theta - mean_item
  )

  # targeting_quality <-
  #   dplyr::case_when(
  #     overlap_index >= 0.90 ~ "Excellent",
  #     overlap_index >= 0.80 ~ "Good",
  #     overlap_index >= 0.70 ~ "Moderate",
  #     TRUE ~ "Poor"
  #   )

  # =====================================
  # SCALE LIMITS
  # =====================================

  x_limits <- range(
    c(theta, df_item$Difficulty),
    na.rm = TRUE
  )

  x_limits <- c(
    x_limits[1] - 0.5,
    x_limits[2] + 0.5
  )

  # =====================================
  # TOP PANEL (HISTOGRAM / BAR)
  # =====================================

  plot_top <-

    ggplot(
      df_person,
      aes(x = Theta)
    ) +

    geom_histogram(
      fill = "grey60",
      colour = "black",
      linewidth = 0.4,
      bins = 30,
      na.rm = TRUE
    ) +

    # Garis geom_vline DIHAPUS dari sini agar tidak menembus bar

    coord_cartesian(
      xlim = x_limits
    )+

    labs(
      y = "Frequency"
    ) +

    theme_classic(base_size = 12) +

    theme(
      legend.position = "none", 
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(), 
      axis.title.y = element_text(face = "bold", colour = "black"),
      axis.text.y = element_text(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )

  # =====================================
  # BOTTOM PANEL (ITEMS)
  # =====================================

  plot_bottom <-

    ggplot(
      df_item,
      aes(
        x = Difficulty,
        y = reorder(Item, Difficulty)
      )
    ) +

    # Garis tetap dipertahankan di panel bawah agar menjadi penanda
    # geom_vline(
    #   xintercept = mean_theta,
    #   colour = "black",
    #   linewidth = 0.6,
    #   linetype = "dashed"
    # ) +

    # geom_vline(
    #   xintercept = mean_item,
    #   colour = "black",
    #   linewidth = 0.6,
    #   linetype = "dotted"
    # ) +

    geom_segment(
      aes(
        x = mean_item,
        xend = Difficulty,
        y = Item,
        yend = Item
      ),
      colour = "grey60",
      linewidth = 0.4
    ) +

    geom_point(
      size = 3,
      colour = "black",
      shape = 17
    ) +

    geom_text_repel(
      aes(label = Item),
      size = 3.5,
      direction = "x",
      box.padding = 0.4,
      point.padding = 0.3,
      segment.color = "grey50",
      max.overlaps = Inf,
      colour = "black"
    ) +

    scale_x_continuous(
      limits = x_limits
    ) +

    labs(
      # x = "Logit Scale (θ / β)",
      y = "Items"
    ) +

    theme_classic(base_size = 12) +

    theme(
      legend.position = "none",
      # axis.title.x = element_text(face = "bold", colour = "black", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", colour = "black", margin = margin(r = 10)),
      axis.text = element_text(colour = "black"),
      # axis.line = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )

  # =====================================
  # COMBINE
  # =====================================

  final_map <-

    plot_top /
    plot_bottom +

    plot_layout(
      heights = c(1, 3)
    ) +

    plot_annotation(
      # title = "Person-Item Wright Map",

      # subtitle =
      #   paste0(
      #     "IPOI = ",
      #     round(overlap_index, 3),
      #     " (",
      #     targeting_quality,
      #     " Targeting); Mean Gap = ",
      #     round(mean_gap, 2),
      #     " logits"
      #   ),

      # caption =
      #   paste(
      #     "Dashed line = Mean Person Ability",
      #     "|",
      #     "Dotted line = Mean Item Difficulty"
      #   ),

      theme = theme(
        plot.title = element_text(face = "bold", size = 14, colour = "black"),
        plot.subtitle = element_text(size = 11, colour = "grey20"),
        plot.caption = element_text(size = 10, colour = "grey30", hjust = 0.5, margin = margin(t = 15)),
        plot.background = element_rect(fill = "white", colour = NA)
      )
    )

  return(final_map)

}