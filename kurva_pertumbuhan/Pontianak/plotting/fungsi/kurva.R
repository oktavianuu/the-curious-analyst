library(ggplot2)
library(ggrepel)
library(dplyr)

kurva_pertumbuhan <- function(
    df,
    who_source,
    local_source,
    show_who = TRUE,       # [NEW] Toggle for WHO data
    show_local = TRUE,     # [NEW] Toggle for Local data
    ylab,
    ylim = NULL,
    xlab = "umur (Tahun)",
    min_umur = NULL,
    max_umur = NULL,
    xbreaks = NULL,
    ybreaks = NULL,
    show_legend = FALSE,
    sex_label = NULL
) {
    
    # [NEW] Safety check: Prevent the function from running if both are FALSE
    if (!show_who && !show_local) {
        stop("Both 'show_who' and 'show_local' are FALSE. There is nothing to plot.")
    }

    # --- 1. Prepare Main Data ---
    plot_df <- df %>%
        mutate(
            # [MODIFIED] Use the boolean toggles to control which sources get mapped
            Model = case_when(
                show_who & Source == who_source   ~ "Model Referensi WHO",
                show_local & Source == local_source ~ "Model Lokal",
                TRUE ~ NA_character_
            )
        ) %>%
        filter(!is.na(Model))

    if (!is.null(min_umur)) {
        plot_df <- plot_df %>%
            filter(umur >= min_umur)
    }
    
    if (!is.null(max_umur)) {
        plot_df <- plot_df %>%
            filter(umur <= max_umur)
    }

    plot_df$Z <- factor(plot_df$Z, levels = c("Z-2", "Z-1", "Z0", "Z1", "Z2"))

    # --- 2. Prepare Label Data (Titik Ujung) ---
    # [NEW] Dynamically decide where to put the Z-score labels at the end of the lines
    # If local is shown, attach to local. If local is hidden, attach to WHO.
    label_target <- if (show_local) "Model Lokal" else "Model Referensi WHO"
    
    label_df <- plot_df %>%
        filter(Model == label_target) %>%
        group_by(Z) %>%
        # [MODIFIED] Use slice_max() instead of filter(max())
        slice_max(order_by = umur, n = 1, with_ties = FALSE) %>%
        ungroup()

    if (is.null(ylim))  ylim <- range(plot_df$Value, na.rm = TRUE)

    if (is.null(xbreaks)) {
        # [MODIFIED] Check if min_umur is provided; if not, use the data's actual minimum
        start_x <- if (!is.null(min_umur)) min_umur else min(plot_df$umur, na.rm = TRUE)
        max_x <- if (!is.null(max_umur)) max_umur else ceiling(max(plot_df$umur, na.rm = TRUE))
        
        # Create the integer sequence
        integers <- seq(ceiling(start_x), max_x, by = 1)
        
        # Combine the starting point with the integers
        xbreaks <- unique(c(start_x, integers))
    }

    if (is.null(ybreaks)) ybreaks <- pretty(ylim)

    # --- 3. Build Plot ---
    p <- ggplot(
        plot_df,
        aes(
            x = umur,
            y = Value,
            group = interaction(Z, Model),
            color = Z,
            linetype = Model
        )
    ) +
        geom_line(linewidth = 0.7, color = "white", lineend = "round") +
        geom_line(linewidth = 0.6, lineend = "round") +
        
        geom_text(
            data = label_df,
            aes(label = Z),
            hjust = -0.3,
            size = 3.5,
            fontface = "bold",
            show.legend = FALSE
        ) +
        
        scale_x_continuous(
            breaks = xbreaks, 
            limits = c(min(xbreaks), max(xbreaks) + 0.8), 
            expand = expansion(mult = c(0, 0.02))
        ) +
        scale_y_continuous(
            limits = ylim, breaks = ybreaks, 
            expand = expansion(mult=c(0, 0.08))
        ) +
        scale_color_manual(
            values = c(
                "Z-2" = "#0072B2", "Z-1" = "#56B4E9", "Z0"  = "#999999",
                "Z1"  = "#E69F00", "Z2"  = "#D55E00"
            ),
            drop = FALSE,
            name = NULL,
            guide = "none" 
        ) +
        scale_linetype_manual(
            values = c(
                "Model Referensi WHO" = "22", 
                "Model Lokal" = "solid"
            ),
            name = NULL
        ) +
        labs(x = xlab, y = ylab) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = 12) +
        theme(
            axis.title.y = element_text(margin=margin(r=6)),
            legend.box = "horizontal",
            legend.position = if (show_legend) c(0.98, 0.02) else "none", 
            legend.justification = c("right", "bottom"),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.key.width = unit(1.2, "cm"), 
            legend.key.height = unit(0.4, "cm"),
            legend.text = element_text(size = 10),

            panel.grid = element_blank(),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5),
            axis.ticks.length = unit(0.15, "cm"),
            axis.text = element_text(size = 10, color = "black"),
            plot.margin = margin(t = 20, r = 35, b = 4, l = 24)
        )

    if (!is.null(sex_label)) {
        p <- p +
            annotation_custom(
                grid::textGrob(
                    sex_label, x = unit(-0.05, "npc"), y = unit(1.015, "npc"),      
                    just = c("left", "bottom"),
                    gp = grid::gpar(fontsize = 12, fontface = "plain")
                )
            )
    }
    
    return(p)
}