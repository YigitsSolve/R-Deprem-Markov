# ============================================================
# 4 BOLGE - 16 DURUM - MARKOV GECIS SIMULASYONU
# ============================================================

# Rastgele zaman verileri olusturma
set.seed(123)

tum_tarihler <- seq(
  from = as.Date("2000-01-01"),
  to   = as.Date("2024-12-31"),
  by   = "day"
)

# Tarih ve bolge sayilari ayni olmali.
zaman <- sample(tum_tarihler, 1000, replace = TRUE)
bolge <- sample(
  c("Bölge 1", "Bölge 2", "Bölge 3", "Bölge 4"),
  1000,
  replace = TRUE
)

deprem_verileri <- data.frame(
  Zaman = zaman,
  Bolge = bolge,
  stringsAsFactors = FALSE
)

# Zamana gore sirala.
deprem_verileri <- deprem_verileri[order(deprem_verileri$Zaman), ]
rownames(deprem_verileri) <- NULL

# StepSize yil cinsindendir.
# 0.1 yaklasik 36.5 gunluk pencereye karsilik gelir.
StepSize <- 0.1

# ------------------------------------------------------------
# 16 durumun tanimi
#
# A = Bolge 1 aktif mi?
# B = Bolge 2 aktif mi?
# C = Bolge 3 aktif mi?
# D = Bolge 4 aktif mi?
#
# State = A + 2*B + 4*C + 8*D
# ------------------------------------------------------------

durum_tablosu <- data.frame(
  State = 0:15,
  A = (0:15) %% 2,
  B = ((0:15) %/% 2) %% 2,
  C = ((0:15) %/% 4) %% 2,
  D = ((0:15) %/% 8) %% 2
)

cat("16 durum:\n")
print(durum_tablosu)

# ------------------------------------------------------------
# Analiz fonksiyonu
# ------------------------------------------------------------

deprem_analizi <- function(deprem_verileri, StepSize = 0.1) {
  if (StepSize <= 0) {
    stop("StepSize sifirdan buyuk olmalidir.")
  }

  gerekli_kolonlar <- c("Zaman", "Bolge")
  if (!all(gerekli_kolonlar %in% names(deprem_verileri))) {
    stop("deprem_verileri veri cercevesinde Zaman ve Bolge kolonlari bulunmalidir.")
  }

  veri <- deprem_verileri[order(deprem_verileri$Zaman), ]
  rownames(veri) <- NULL

  # Aylarin 30 gun kabul edilmesi yerine Date sinifi kullanilir.
  pencere_gun <- max(1L, round(365.25 * StepSize))

  pencere_baslangiclari <- seq(
    from = min(veri$Zaman),
    to   = max(veri$Zaman),
    by   = pencere_gun
  )

  state_detay <- data.frame(
    Baslangic = pencere_baslangiclari,
    Bitis = pencere_baslangiclari + pencere_gun,
    A = integer(length(pencere_baslangiclari)),
    B = integer(length(pencere_baslangiclari)),
    C = integer(length(pencere_baslangiclari)),
    D = integer(length(pencere_baslangiclari)),
    State = integer(length(pencere_baslangiclari))
  )

  for (i in seq_along(pencere_baslangiclari)) {
    pencere_bas <- pencere_baslangiclari[i]
    pencere_bit <- pencere_bas + pencere_gun

    # [baslangic, bitis) kullanilir; sinirdaki kayit iki pencereye girmez.
    pencere_verisi <- veri[
      veri$Zaman >= pencere_bas & veri$Zaman < pencere_bit,
      ,
      drop = FALSE
    ]

    A <- as.integer(any(pencere_verisi$Bolge == "Bölge 1"))
    B <- as.integer(any(pencere_verisi$Bolge == "Bölge 2"))
    C <- as.integer(any(pencere_verisi$Bolge == "Bölge 3"))
    D <- as.integer(any(pencere_verisi$Bolge == "Bölge 4"))

    state <- A + 2 * B + 4 * C + 8 * D

    state_detay$A[i] <- A
    state_detay$B[i] <- B
    state_detay$C[i] <- C
    state_detay$D[i] <- D
    state_detay$State[i] <- state
  }

  # MATLAB kodundaki G dizisinin R karsiligi.
  G <- state_detay$State

  # 16x16 gecis frekans matrisi.
  BG <- matrix(
    0L,
    nrow = 16,
    ncol = 16,
    dimnames = list(
      paste0("State_", 0:15),
      paste0("State_", 0:15)
    )
  )

  # G(t-1) -> G(t) gecislerini say.
  if (length(G) >= 2) {
    for (i in 2:length(G)) {
      onceki_state <- G[i - 1]
      sonraki_state <- G[i]

      BG[onceki_state + 1, sonraki_state + 1] <-
        BG[onceki_state + 1, sonraki_state + 1] + 1L
    }
  }

  # Gecis frekanslarini satir bazinda olasiliga cevir.
  P <- matrix(
    0,
    nrow = 16,
    ncol = 16,
    dimnames = dimnames(BG)
  )

  satir_toplamlari <- rowSums(BG)
  dolu_satirlar <- satir_toplamlari > 0

  if (any(dolu_satirlar)) {
    P[dolu_satirlar, ] <-
      BG[dolu_satirlar, , drop = FALSE] /
      satir_toplamlari[dolu_satirlar]
  }

  list(
    Durumlar = durum_tablosu,
    StateDetay = state_detay,
    G = G,
    GecisFrekans = BG,
    GecisOlasilik = P
  )
}

# ------------------------------------------------------------
# Analizi calistir
# ------------------------------------------------------------

sonuc <- deprem_analizi(deprem_verileri, StepSize)

# Eski kodla uyumluluk icin frekans matrisini "matris" degiskeninde de tut.
matris <- sonuc$GecisFrekans

cat("\nZaman pencereleri ve durumlar:\n")
print(sonuc$StateDetay)

cat("\n16x16 Gecis Frekans Matrisi:\n")
print(sonuc$GecisFrekans)

cat("\n16x16 Markov Gecis Olasilik Matrisi:\n")
print(round(sonuc$GecisOlasilik, 3))

# ------------------------------------------------------------
# Isi haritalari
# ------------------------------------------------------------

heatmap(
  sonuc$GecisFrekans,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  margins = c(7, 7),
  main = "16 Durumlu Gecis Frekans Matrisi"
)

heatmap(
  sonuc$GecisOlasilik,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  margins = c(7, 7),
  main = "16 Durumlu Markov Gecis Olasilik Matrisi"
)
