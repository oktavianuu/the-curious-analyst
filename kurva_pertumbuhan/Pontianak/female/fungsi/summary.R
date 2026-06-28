library(dplyr)

summarize_categorical <- function(data, var, digits = 1) {
  
  # 1. Hitung frekuensi dan persentase
  result <- data %>%
    count(Category = .data[[var]], name = "Freq") %>%
    mutate(
      Category = as.character(Category),
      # Ubah nilai missing (NA) menjadi teks "NA" agar terlihat di tabel
      Category = if_else(is.na(Category), "NA", Category),
      
      # Hitung kolom persentase terpisah
      Percentage = round(100 * Freq / sum(Freq), digits)
    )

  # 2. Buat baris Total dengan struktur yang konsisten
  total_row <- tibble::tibble(
    Category = "Total",
    Freq = sum(result$Freq),
    Percentage = 100
  )

  # 3. Gabungkan data utama dengan baris Total
  bind_rows(
    result,
    total_row
  )
}

# Example usage: 
# summarize_categorical(female_compare, "cat_manual")

summarize_continuous <- function(data, var, digits = 3) {
  
  x <- data[[var]]
  
  # Isolate the math from the text formatting
  x_mean = mean(x, na.rm = TRUE)
  x_sd   = sd(x, na.rm = TRUE)
  x_med  = median(x, na.rm = TRUE)
  x_q25  = quantile(x, 0.25, na.rm = TRUE)
  x_q75  = quantile(x, 0.75, na.rm = TRUE)
  x_min  = min(x, na.rm = TRUE)
  x_max  = max(x, na.rm = TRUE)
  x_n    = sum(!is.na(x))
  
  # Construct the final table cleanly
  tibble(
    Statistic = c("N", "Mean ± SD", "Median (IQR)", "Minimum", "Maximum"),
    Value = c(
      as.character(x_n),
      sprintf("%.*f ± %.*f", digits, x_mean, digits, x_sd),
      sprintf("%.*f (%.*f, %.*f)", digits, x_med, digits, x_q25, digits, x_q75),
      as.character(round(x_min, digits)),
      as.character(round(x_max, digits))
    )
  )
}

# Example usage: 
# summarize_continuous(female_compare, "bmi")