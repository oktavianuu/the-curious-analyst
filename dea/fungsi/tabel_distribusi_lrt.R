tabel_distribusi_lrt <- function(data_matrix, item_names, splitcr = "mean") {
  
  # 1. Hitung total skor untuk dasar pemisahan
  total_skor <- rowSums(data_matrix[, item_names], na.rm = TRUE)
  
  # 2. Logika Pemisah (Split Criterion): Mean atau Median
  if (splitcr == "mean") {
    batas <- mean(total_skor, na.rm = TRUE)
  } else if (splitcr == "median") {
    batas <- median(total_skor, na.rm = TRUE)
  } else {
    stop("Argumen splitcr keliru. Harap masukkan 'mean' atau 'median'.")
  }
  
  # 3. Belah kelompok berdasarkan batas yang dipilih
  kelompok <- ifelse(total_skor < batas, "Rendah", "Tinggi")
  
  # 4. Fungsi Mesin Pemroses Item (Internal)
  hitung_komparasi_item <- function(item_name) {
    
    vektor_item <- data_matrix[[item_name]]
    
    if (length(kelompok) != length(vektor_item)) {
      stop(paste("Panjang data beda di soal:", item_name))
    }
    
    f_tabel <- table(factor(kelompok, levels = c("Rendah", "Tinggi")), 
                     factor(vektor_item, levels = 0:4))
    
    p_tabel <- prop.table(f_tabel, 1) * 100
    
    # Auto-Detector: Beri flag bintang jika ada sel bernilai 0
    simbol_flag <- ifelse(any(f_tabel == 0), "*", "")
    
    # Rakit baris dan tambahkan spasi kosong sebagai pembatas antar-soal
    baris_rendah <- c(sprintf("%d (%.1f%%)", f_tabel[1, ], p_tabel[1, ]), simbol_flag)
    baris_tinggi <- c(sprintf("%d (%.1f%%)", f_tabel[2, ], p_tabel[2, ]), "") 
    baris_batas  <- rep("", 6)
    
    gabung_teks <- rbind(baris_rendah, baris_tinggi, baris_batas)
    
    # Trik ilusi Rowname yang dinamis menggunakan deret spasi
    posisi_urut <- which(item_names == item_name)
    nama_kosong <- paste(rep(" ", posisi_urut), collapse = "")
    
    rownames(gabung_teks) <- c(paste0(item_name, " - Rendah"), 
                               paste0(item_name, " - Tinggi"), 
                               nama_kosong)
                               
    colnames(gabung_teks) <- c(paste("Skor", 0:4), "Status")
    
    return(gabung_teks)
  }
  
  # 5. Lakukan iterasi otomatis dan satukan hasil
  daftar_tabel <- lapply(item_names, hitung_komparasi_item)
  tabel_final <- do.call(rbind, daftar_tabel)
  
  return(tabel_final)
}