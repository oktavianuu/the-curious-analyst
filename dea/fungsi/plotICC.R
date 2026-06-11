plot_icc_clean <- function(model_object,
                           item_name) {

  # =========================
  # SAVE GRAPHICS STATE
  # =========================

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(
    mar = c(5, 4, 4, 4)
  )

  # =========================
  # CATEGORY COLORS
  # =========================

  warna_kurva <- c(
    "#00468B",  # kategori 0
    "#0099B4",  # kategori 1
    "#925E9F",  # kategori 2
    "#FDAF91",  # kategori 3
    "#ED0000",  # kategori 4
    "#42B540",  # kategori 5
    "#7E57C2",  # kategori 6
    "#FFB300"   # kategori 7
  )

  # =========================
  # THRESHOLD COLORS
  # δ1 mengikuti kategori 1
  # δ2 mengikuti kategori 2
  # dst
  # =========================

  warna_threshold <- c(
    "1" = "#0099B4",
    "2" = "#925E9F",
    "3" = "#FDAF91",
    "4" = "#ED0000",
    "5" = "#42B540",
    "6" = "#7E57C2",
    "7" = "#FFB300"
  )

  # =========================
  # EXTRACT THRESHOLDS
  # =========================

  thresh_raw <- thresholds(model_object)$threshtable[[1]][item_name, ]

  thresh_raw <- thresh_raw[
    names(thresh_raw) != "Location"
  ]

  thresh_clean <- thresh_raw[
    !is.na(thresh_raw)
  ]

  thresh_df <- data.frame(
    tau   = names(thresh_clean),
    value = as.numeric(thresh_clean),
    stringsAsFactors = FALSE
  )

  # =========================
  # AUTO X RANGE
  # =========================

  xmin <- min(thresh_df$value) - 1.5
  xmax <- max(thresh_df$value) + 1.5

  xmin <- min(xmin, -4)
  xmax <- max(xmax,  4)

  # =========================
  # PLOT ICC
  # =========================

  plotICC(
    model_object,
    item.subset = item_name,
    col = warna_kurva,
    lwd = 3,
    legpos = FALSE,
    xlim = c(xmin, xmax),
    main = paste(
      "Item Characteristic Curve:",
      item_name
    )
  )

  usr <- par("usr")

  # =========================
  # THRESHOLD LABEL POSITIONS
  # =========================

  n_tau <- nrow(thresh_df)

  posisi_y <- seq(
    0.15,
    0.85,
    length.out = n_tau
  )

  # =========================
  # DRAW THRESHOLDS
  # =========================

  for(i in seq_len(n_tau)) {

    nomor_tau <- gsub(
      "[^0-9]",
      "",
      thresh_df$tau[i]
    )

    warna_tau <- warna_threshold[nomor_tau]

    # garis threshold

    segments(
      x0 = thresh_df$value[i],
      y0 = usr[3],
      x1 = thresh_df$value[i],
      y1 = usr[4],
      col = warna_tau,
      lty = 2,
      lwd = 1.5
    )

    # label threshold

    text(
      x = thresh_df$value[i],
      y = posisi_y[i],
      labels = bquote(
        delta[.(nomor_tau)] ==
          .(round(thresh_df$value[i], 2))
      ),
      pos = 2,
      cex = 0.95,
      font = 2,
      col = warna_tau
    )
  }

  # =========================
  # LEGEND
  # =========================

  jumlah_kategori <- n_tau + 1

  legend(
    "topright",
    legend = as.character(
      0:(jumlah_kategori - 1)
    ),
    col = warna_kurva[
      seq_len(jumlah_kategori)
    ],
    lwd = 3,
    bty = "o",
    # bg = "white",
    # title = "Kategori",
    cex = 1
  )
}