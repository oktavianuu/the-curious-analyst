plot_wright_map <- function(model_object) {

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

  sd_theta <- sd(
    theta,
    na.rm = TRUE
  )

  df_person$Level <- cut(
    df_person$Theta,
    breaks = c(
      -Inf,
      mean_theta - sd_theta,
      mean_theta + sd_theta,
      Inf
    ),
    labels = c(
      "Low",
      "Moderate",
      "High"
    )
  )

  df_person$Level <- factor(
    df_person$Level,
    levels = c(
      "High",
      "Moderate",
      "Low"
    )
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

  targeting_quality <-
    dplyr::case_when(
      overlap_index >= 0.90 ~ "Excellent",
      overlap_index >= 0.80 ~ "Good",
      overlap_index >= 0.70 ~ "Moderate",
      TRUE ~ "Poor"
    )

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
  # ITEM COVERAGE
  # =====================================

  item_min <- min(
    df_item$Difficulty,
    na.rm = TRUE
  )

  item_max <- max(
    df_item$Difficulty,
    na.rm = TRUE
  )

  # =====================================
  # TOP PANEL
  # =====================================

  plot_top <-

    ggplot(
      df_person,
      aes(x = Theta)
    ) +

    annotate(
      "rect",
      xmin = item_min,
      xmax = item_max,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey50",
      alpha = 0.04
    ) +

    geom_density(
      aes(fill = Level),
      alpha = 0.4,
      linewidth = 0.4
    ) +

    geom_vline(
      xintercept = mean_theta,
      colour = "#2E86C1",
      linewidth = 0.5,
      linetype = "dashed"
    ) +

    geom_vline(
      xintercept = mean_item,
      colour = "#C0392B",
      linewidth = 0.5,
      linetype = "dashed"
    ) +

    scale_fill_manual(
      name = "Person Ability\nLevel",
      values = c(
        "High" = "#E74C3C",
        "Moderate" = "#F1C40F",
        "Low" = "#3498DB"
      )
    ) +

    scale_x_continuous(
      limits = x_limits
    ) +

    labs(
      y = "Density"
    ) +

    theme_minimal(base_size = 12) +

    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  # =====================================
  # BOTTOM PANEL
  # =====================================

  plot_bottom <-

    ggplot(
      df_item,
      aes(
        x = Difficulty,
        y = reorder(Item, Difficulty)
      )
    ) +

    annotate(
      "rect",
      xmin = item_min,
      xmax = item_max,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey50",
      alpha = 0.06
    ) +

    geom_vline(
      xintercept = mean_theta,
      colour = "#2E86C1",
      linewidth = 0.55,
      linetype = "dashed"
    ) +

    geom_vline(
      xintercept = mean_item,
      colour = "#C0392B",
      linewidth = 0.55,
      linetype = "dashed"
    ) +

    geom_segment(
      aes(
        x = mean_item,
        xend = Difficulty,
        y = Item,
        yend = Item
      ),
      colour = "grey75",
      linewidth = 0.35
    ) +

    geom_point(
      size = 3.2,
      colour = "#C0392B"
    ) +

    geom_text_repel(
      aes(label = Item),
      size = 3,
      direction = "x",
      box.padding = 0.35,
      point.padding = 0.25,
      segment.color = "grey60",
      max.overlaps = Inf
    ) +

    scale_x_continuous(
      limits = x_limits
    ) +

    labs(
      x = "Logit Scale (θ / β)",
      y = "Items"
    ) +

    theme_classic(base_size = 12) +

    theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
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
      title = "Person-Item Wright Map",

      subtitle =
        paste0(
          "IPOI = ",
          round(overlap_index, 3),
          " (",
          targeting_quality,
          " Targeting); Mean Gap = ",
          round(mean_gap, 2),
          " logits"
        ),

      caption =
        paste(
          "Blue dashed line = Mean Person Ability",
          "|",
          "Red dashed line = Mean Item Difficulty",
          "|",
          "Shaded region = Item Difficulty Coverage"
        )
    )

  return(final_map)

}