# Rastgele zaman verileri oluşturma
set.seed(123) # Rastgelelik için seed belirleme
zaman <- as.Date(seq(as.Date("2000-01-01"), as.Date("2024-12-31"), by = "day"), origin = "1970-01-01") # 2000-2024 arası rastgele zamanlar oluşturma
zaman <- sample(zaman, 100, replace = TRUE) # 100 adet rastgele zaman seçme

# Rastgele bölge verileri oluşturma
bölge <- sample(c("Bölge 1", "Bölge 2", "Bölge 3", "Bölge 4"), 1000, replace = TRUE)

# Verileri bir veri çerçevesine dönüştürme
deprem_verileri <- data.frame(Zaman = zaman, Bölge = bölge)

# Veri çerçevesini yazdırma
print(deprem_verileri)

# Zamanı küçükten büyüğe sıralama
deprem_verileri <- deprem_verileri[order(deprem_verileri$Zaman), ]

# tidyverse kütüphanesi
library(tidyverse)

# Yıl, ay ve gün olarak ayıralım
deprem_verileri <- deprem_verileri %>%
  separate(Zaman, into = c("Yıl", "Ay", "Gün"), sep = "-")

# Yıl, ay ve gün bilgilerini sayısal değere dönüştürme
deprem_verileri$Yıl <- as.numeric(deprem_verileri$Yıl)
deprem_verileri$Ay <- as.numeric(deprem_verileri$Ay)
deprem_verileri$Gün <- as.numeric(deprem_verileri$Gün)

# Sürekli zaman dönüşümü
deprem_verileri$Zaman_surekli <- deprem_verileri$Yıl + ((deprem_verileri$Ay - 1) * 30 + deprem_verileri$Gün) / 365

# DepremDf oluşturma
DepremDf <- deprem_verileri[, c("Bölge", "Zaman_surekli")]

# row.names sütununu kaldırma
rownames(DepremDf) <- NULL

# StepSize değişkenini belirleme
StepSize <- 0.1

# Fonksiyon tanımlama
deprem_analizi <- function(deprem_verileri) {
  adim_sayisi <- floor(365 * StepSize / 2)  # Adım sayısını hesaplama
  matris <- matrix(0, nrow = 16, ncol = 16)  # Boş bir 16x16 matris oluşturma
  for (i in seq(adim_sayisi, nrow(deprem_verileri), by = adim_sayisi)) {  # Belirlenen adımlarla döngü oluşturma
    zaman <- deprem_verileri$Zaman_surekli[i]  # Mevcut zamanı alıyoruz
    deprem_bolgeleri <- unique(deprem_verileri$Bölge[deprem_verileri$Zaman_surekli >= zaman - StepSize & deprem_verileri$Zaman_surekli < zaman])  # Belirli bir zaman aralığındaki benzersiz bölgeleri alıyoruz
    durumlar <- match(deprem_bolgeleri, paste0("Bölge ", 1:4))  # Bölge etiketlerini sayısal değerlere dönüştürüyoruz
    matris[durumlar, durumlar] <- matris[durumlar, durumlar] + 1  # Matrisi güncelliyoruz
  }
  return(matris)  # Son matrisi döndürme
}

# Fonksiyonu kullanarak matrisi oluşturma
matris <- deprem_analizi(DepremDf)

# Başlık ekleyelim
cat("Deprem Analizi Matrisi:\n")

# Matrisi yazdırma
print(matris)


# Isı haritası oluşturma
heatmap(matris)


