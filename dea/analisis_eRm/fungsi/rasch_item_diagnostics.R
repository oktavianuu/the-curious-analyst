evaluate_item_fit <- function(rasch_model) {
  
  # =======================================================
  # 1. EKSTRAKSI PARAMETER LANGSUNG DARI MODEL
  # =======================================================
  threshold_estimates <- thresholds(rasch_model)
  person_estimates    <- person.parameter(rasch_model)
  fit_statistics      <- itemfit(person_estimates)
  
  # =======================================================
  # 2. EKSTRAKSI KALIBRASI MATEMATIS (LOCATION)
  # =======================================================
  # Ekstrak matriks threshold
  thresh_matrix <- threshold_estimates$threshtable[[1]]
  
  # KONVERSI EKSPLISIT: Mengubah matriks menjadi data frame agar operator $ berfungsi
  thresh_df      <- as.data.frame(thresh_matrix)
  
  item_locations <- thresh_df$Location
  item_names     <- rownames(thresh_df)
  
  # =======================================================
  # 3. PENYUSUNAN MATRIKS DIAGNOSTIK
  # =======================================================
  diagnostic_matrix <- data.frame(
    Item        = item_names,
    Calibration = as.numeric(item_locations),
    Infit_MSQ   = as.numeric(fit_statistics$i.infitMSQ),
    Outfit_MSQ  = as.numeric(fit_statistics$i.outfitMSQ),
    Infit_ZSTD  = as.numeric(fit_statistics$i.infitZ),
    Outfit_ZSTD = as.numeric(fit_statistics$i.outfitZ),
    PTMEA       = as.numeric(fit_statistics$i.disc),
    Discrim     = as.numeric(fit_statistics$i.disc)
  )
  
  # =======================================================
  # 4. PRESISI DESIMAL (4 ANGKA)
  # =======================================================
  numeric_columns <- c("Calibration", "Infit_MSQ", "Outfit_MSQ", 
                       "Infit_ZSTD", "Outfit_ZSTD", "PTMEA", "Discrim")
  diagnostic_matrix[numeric_columns] <- lapply(diagnostic_matrix[numeric_columns], round, 4)
  
  # =======================================================
  # 5. KALKULASI STATISTIK DESKRIPTIF (MEAN & SD)
  # =======================================================
  mean_cal <- round(mean(diagnostic_matrix$Calibration, na.rm = TRUE), 4)
  sd_cal   <- round(sd(diagnostic_matrix$Calibration, na.rm = TRUE), 4)
  
  summary_template <- diagnostic_matrix[1, ]
  summary_template[1, ] <- NA
  
  row_mean <- summary_template
  row_mean$Item <- "MEAN"
  row_mean$Calibration <- mean_cal
  
  row_sd <- summary_template
  row_sd$Item <- "SD"
  row_sd$Calibration <- sd_cal
  
  # =======================================================
  # 6. INTEGRASI HASIL AKHIR
  # =======================================================
  final_table <- rbind(diagnostic_matrix, row_mean, row_sd)
  rownames(final_table) <- NULL 
  
  return(final_table)
}


classify_difficulty_sumintono <- function(item_vector, logit_vector) {
  
  # =======================================================
  # 1. VALIDASI INTEGRITAS DATA
  # =======================================================
  if (!is.numeric(logit_vector)) {
    stop("Galat: Input 'logit_vector' harus berupa data numerik (angka).")
  }
  
  # =======================================================
  # 2. EKSTRAKSI PARAMETER EMPIRIS (DYNAMIC ANCHORS)
  # =======================================================
  mean_items <- mean(logit_vector, na.rm = TRUE)
  sd_items   <- sd(logit_vector, na.rm = TRUE)
  
  # =======================================================
  # 3. DIAGNOSTIK CONSOLE (TRANSPARANSI BATAS)
  # =======================================================
  # Mencetak informasi ke console agar analis bisa melihat batas absolut
  cat("\n==================================================\n")
  cat("       SUMINTONO & WIDHIARSO CALIBRATION\n")
  cat("==================================================\n")
  cat(sprintf(" Empirical Mean (\U03BC) : %+.4f Logit\n", mean_items))
  cat(sprintf(" Empirical SD (\U03C3)   : %.4f Logit\n", sd_items))
  cat("--------------------------------------------------\n")
  cat(sprintf("Very Difficult  : Logit > %+.4f\n", mean_items + sd_items))
  cat(sprintf("Difficult       : %+.4f < Logit <= %+.4f\n", mean_items, mean_items + sd_items))
  cat(sprintf("Easy            : %+.4f <= Logit <= %+.4f\n", mean_items - sd_items, mean_items))
  cat(sprintf("Very Easy       : Logit < %+.4f\n", mean_items - sd_items))
  cat("==================================================\n\n")

  # =======================================================
  # 4. STANDARISASI METRIK (TRUE Z-SCORE DISTANCE)
  # =======================================================
  standardized_distance <- (logit_vector - mean_items) / sd_items
  
  # =======================================================
  # 5. ALGORITMA KLASIFIKASI (EVALUASI Z-SCORE)
  # =======================================================
  difficulty_mapping <- ifelse(standardized_distance > 1, "Very Difficult",
                        ifelse(standardized_distance > 0 & standardized_distance <= 1, "Difficult",
                        ifelse(standardized_distance >= -1 & standardized_distance <= 0, "Easy", 
                               "Very Easy")))
                               
  # =======================================================
  # 6. PENJELASAN LOGIKA (BUKTI INTERVAL)
  # =======================================================
  logic_explanation <- ifelse(standardized_distance > 1, "> +1.00 SD",
                       ifelse(standardized_distance > 0 & standardized_distance <= 1, "0.00 s/d +1.00 SD",
                       ifelse(standardized_distance >= -1 & standardized_distance <= 0, "-1.00 s/d 0.00 SD", 
                              "< -1.00 SD")))

  # =======================================================
  # 7. KOMPILASI MATRIKS HASIL
  # =======================================================
  academic_table <- data.frame(
    Item          = item_vector,
    Logit         = round(logit_vector, 4),
    `Z-Score`     = round(standardized_distance, 2),
    Interval      = logic_explanation,
    Classification  = difficulty_mapping,
    check.names   = FALSE
  )
  
  # Mengurutkan berdasarkan tingkat kesukaran tertinggi (descending)
  academic_table <- academic_table[order(-academic_table$Logit), ]
  rownames(academic_table) <- NULL
  
  return(academic_table)
}
