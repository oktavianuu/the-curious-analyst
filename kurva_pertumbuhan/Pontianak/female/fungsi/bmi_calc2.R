library(dplyr)
library(tidyr)
library(anthroplus)

# =====================================================
# 1. HELPER FUNCTIONS (Internal Logic)
# =====================================================

# Fungsi Kategorisasi Z-score Standar Kemenkes RI/WHO 2007 (Usia 5-19 Tahun)
categorize_bmi_z <- function(z) {
  status <- dplyr::case_when(
    # 1. Karantina data anomali di awal (NA, NaN, Inf)
    is.na(z) | is.nan(z) | is.infinite(z) ~ NA_character_,
    
    # 2. Evaluasi berjenjang (Top-Down)
    z < -3.0  ~ "Gizi Buruk",
    z < -2.0  ~ "Gizi Kurang",
    z <= 1.0  ~ "Gizi Normal",
    z <= 2.0  ~ "Gizi Lebih",
    z > 2.0   ~ "Obesitas",
    
    # 3. Safety net absolut (jika ada angka yang lolos dari kondisi di atas)
    TRUE      ~ NA_character_ 
  )
  
  # Kunci dalam struktur ordinal (bertingkat)
  factor(
    status, 
    levels = c("Gizi Buruk", "Gizi Kurang", "Gizi Normal", "Gizi Lebih", "Obesitas"),
    ordered = TRUE
  )
}

# Calculates statistical error between two columns
calculate_metrics <- function(df, pred_col, ref_col, label = "comparison") {
  x <- df[[pred_col]]
  y <- df[[ref_col]]
  
  tibble(
    model       = label,
    correlation = cor(x, y, use = "complete.obs"),
    mae         = mean(abs(x - y), na.rm = TRUE),
    rmse        = sqrt(mean((x - y)^2, na.rm = TRUE)),
    max_error   = max(abs(x - y), na.rm = TRUE)
  ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 6)))
}

# =====================================================
# 2. CORE CALCULATION FUNCTION
# =====================================================

calculate_bmi_zscores <- function(data, target_sex = 2) {
  
  # 1. Fetch WHO LMS Table dynamically based on sex
  who_lms <- anthroplus:::bfa_growth_standards %>%
    filter(sex == target_sex) %>%
    arrange(age)
  
  # 2. BASE DATA: Lock original table and stamp with an ID
  # TAMBAHAN: umur_bulan dihitung di sini agar otomatis terbawa ke output akhir
  base_data <- data %>%
    mutate(
      record_id = row_number(),
      umur_bulan = as.numeric(umur) * 12
    )
  
  # 3. CALCULATION SUBSET: Branch off to do the math safely
  calc_subset <- base_data %>%
    # Only calculate on complete records to prevent math errors
    filter(!is.na(bb), !is.na(tb), !is.na(umur)) %>%
    mutate(
      bb = as.numeric(bb),
      tb = as.numeric(tb),
      # umur_bulan sudah tidak perlu dihitung lagi di sini
      bmi = bb / (tb / 100)^2,
      age_months_who = round(umur_bulan) 
    ) %>%
    left_join(
      select(who_lms, age, l_manual = l, m_manual = m, s_manual = s),
      by = c("age_months_who" = "age")
    ) %>%
    mutate(
      l_interp = approx(x = who_lms$age, y = who_lms$l, xout = umur_bulan)$y,
      m_interp = approx(x = who_lms$age, y = who_lms$m, xout = umur_bulan)$y,
      s_interp = approx(x = who_lms$age, y = who_lms$s, xout = umur_bulan)$y,
      
      bmi_z_manual = ifelse(
        l_manual == 0,
        log(bmi / m_manual) / s_manual,
        ((bmi / m_manual)^l_manual - 1) / (l_manual * s_manual)
      ),
      
      bmi_z_interp = ifelse(
        l_interp == 0,
        log(bmi / m_interp) / s_interp,
        ((bmi / m_interp)^l_interp - 1) / (l_interp * s_interp)
      )
    )

  # 4. ANTHROPLUS PACKAGE: Execute on the subset
  who_pkg <- anthroplus_zscores(
    sex = rep(target_sex, nrow(calc_subset)),
    age_in_months = calc_subset$umur_bulan,
    height_in_cm = calc_subset$tb,
    weight_in_kg = calc_subset$bb
  )

  # Lock the results to the subset
  calc_subset$bmi_z_package <- who_pkg$zbfa

  # 5. ISOLATE RESULTS: Extract only the ID and our new columns
  results_only <- calc_subset %>%
    mutate(
      diff_manual_pkg = bmi_z_manual - bmi_z_package,
      diff_interp_pkg = bmi_z_interp - bmi_z_package
    ) %>%
    mutate(
      across(
        .cols = starts_with("bmi_z_"),
        .fns  = categorize_bmi_z,
        .names = "cat_{sub('bmi_z_', '', .col)}"
      )
    ) %>%
    # TAMBAHAN: Rename 'bmi' hasil hitungan kalkulator menjadi 'bmi_who' SEBELUM digabung
    select(
      record_id, 
      bmi_who = bmi,
      starts_with("bmi_z_"),
      starts_with("diff_"),
      starts_with("cat_")
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 6)))

  # 6. THE MERGE: Reattach the results to the pristine original data
  final_output <- base_data %>%
    left_join(results_only, by = "record_id") %>%
    # TAMBAHAN: Ubah nama 'bmi' dari dataset orisinal menjadi 'bmi_empiris'
    # any_of digunakan agar kode tetap aman (tidak error) jika ternyata dataset awal tidak punya kolom 'bmi'
    rename(any_of(c(bmi_empiris = "bmi"))) %>% 
    select(-record_id)
    
  return(final_output)
}

# =====================================================
# 3. CORE VALIDATION FUNCTION
# =====================================================

validate_bmi_zscores <- function(calculated_data) {
  
  # Runs the helper metric function on the calculated dataset
  results <- bind_rows(
    calculate_metrics(calculated_data, "bmi_z_manual", "bmi_z_package", "Manual vs Anthro"),
    calculate_metrics(calculated_data, "bmi_z_interp", "bmi_z_package", "Interpolated vs Anthro")
  )
  
  return(results)
}