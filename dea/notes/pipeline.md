# ==============================================================================
# PIPELINE ANALISIS RASCH POLITOMUS (PARTIAL CREDIT MODEL)
# ==============================================================================

# 1. LOAD PACKAGES
# Pastikan kedua paket ini sudah terinstal di sistem Anda
library(eRm)
library(psych)

# [Asumsi: data_matrix adalah matriks/data.frame yang berisi respons subjek]
# Pastikan data_matrix hanya berisi kolom item (numerik, dimulai dari skor 0, 1, 2, dst.)
# data_matrix <- read.csv("data_kamu.csv") 

# ==============================================================================
# TAHAP 1: ESTIMASI MODEL UTAMA
# ==============================================================================

# Estimasi Parameter Item menggunakan Partial Credit Model
rasch_model <- PCM(data_matrix)
summary(rasch_model)


# ==============================================================================
# TAHAP 2: UJI ASUMSI GLOBAL & INVARIANSI PARAMETER
# ==============================================================================

# Andersen's Likelihood Ratio Test (Uji Kelayakan Model Secara Global)
# Membagi sampel berdasarkan nilai median skor total untuk menguji konstansi parameter
uji_lr <- LRtest(rasch_model, splitcr = "median")
print(uji_lr)

# Visualisasi Invariansi Parameter (Goodness-of-Fit Plot)
# Item yang berada jauh dari garis diagonal mengindikasikan pelanggaran invariansi
plotGOF(uji_lr, ctrline = list(lty = "dotted", col = "red"), 
        main = "Andersen's LR Test - Graphical Model Check")


# ==============================================================================
# TAHAP 3: DIAGNOSTIK THRESHOLD (AMBANG BATAS KATEGORI SKOR)
# ==============================================================================

# Menghitung parameter ambang batas (threshold) untuk setiap item politomus
param_threshold <- thresholds(rasch_model)
print(param_threshold)

# Memeriksa urutan threshold (mendeteksi disordered thresholds)
# Jika ada kategori yang tumpang tindih, urutan penskoran item bermasalah
# Visualisasi untuk 5 item pertama sebagai sampel:
plotICC(rasch_model, item.subset = 1:min(5, ncol(data_matrix)), ask = FALSE)


# ==============================================================================
# TAHAP 4: ESTIMASI PARAMETER KEMAMPUAN PERSON
# ==============================================================================

# Estimasi parameter kemampuan (ability) person menggunakan Maximum Likelihood
person_param <- person.parameter(rasch_model)


# ==============================================================================
# TAHAP 5: UJI KECOCOKAN LOKAL (ITEM FIT & PERSON FIT DIAGNOSTICS)
# ==============================================================================

# 1. Item Fit (Melihat MSQ Infit/Outfit dan t-statistic)
eval_itemfit <- itemfit(person_param)
print(eval_itemfit)

# 2. Person Fit (Melihat apakah ada subjek dengan pola respons aneh/tebakan)
eval_personfit <- personfit(person_param)
# Menampilkan 10 person pertama sebagai sampel
head(data.frame(Infit_MSQ = eval_personfit$i.outfit, Outfit_MSQ = eval_personfit$o.outfit), 10)


# ==============================================================================
# TAHAP 6: RELIABILITAS SEPARASI
# ==============================================================================

# Menghitung Person Separation Reliability (sebanding dengan Cronbach's Alpha / Wright's Separation)
reliabilitas <- SepRel(person_param)
cat("Separation Reliability Score:", reliabilitas, "\n")


# ==============================================================================
# TAHAP 7: UJI UNIDIMENSI (PENDEKATAN GANDA)
# ==============================================================================

# Pendekatan A: Martin-Löf Test (Uji homogenitas item berbasis pemisahan sub-skala)
uji_mloef <- MLoef(rasch_model, splitcr = "median")
print(uji_mloef)

# Pendekatan B: Principal Component Analysis of Residuals (PCAR)
# Mengekstrak matriks residu dari person parameter
matriks_residu <- residuals(person_param)

# Menjalankan PCA pada residu untuk mendeteksi dimensi sekunder (faktor pengganggu)
pca_residu <- principal(matriks_residu, nfactors = 3, rotate = "none")

# Tampilkan Eigenvalues dari residu
# Rule of thumb: Jika kontras pertama (Eigenvalue terbesar pertama) < 2.0, 
# maka asumsi unidimensi terpenuhi (tidak ada dimensi laten sekunder yang dominan).
cat("Eigenvalues dari Residu PCA:\n")
print(pca_residu$values[1:3])