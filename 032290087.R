# Veriyi yukleme
data_f <- read.csv("mosquito_Indicator.csv")

# Veriyi kontrok edelim
data_f

# Tarih sütununu Date formatına
data_f$date <- as.Date(data_f$date)

# 2017 yılına ait verileri filtreleyelim
data <- subset(data_f, format(data_f$date, "%Y") == "2017")
head(data)

# mean_T sütunu için özet bilgi
summary(data$mean_T)

# Ay bilgisini ekleyelim
data$month <- format(data$date, "%m")
head(data)


# İndeksleri sıfırlamak için ekledim
rownames(data) <- NULL
head(data)

# Her ayın ortalama sıcaklığını hesaplayalım
monthly_avg_temp <- tapply(data$mean_T, data$month, mean)
monthly_avg_temp


# En yüksek ortalama sıcaklığa sahip ayı bulalım
G1 <- which.max(monthly_avg_temp)
G1

# Sıcaklık < 0°C olduğu günlerin indekslerini bulalım
G2 <- which(data$mean_T < 0)
G2

# Sıcaklıkları dönüştürelim
data$G3_sicaklikFahrenheit <- (data$mean_T * 9/5) + 32

# Sonuçları gözden geçirelim
head(data[, c("date", "mean_T", "G3_sicaklikFahrenheit")])
data

# 25°C'nin üzerindeki gün sayısını bulalım
G4Bonus <- sum(data$mean_T > 25)
G4Bonus

# Günlük sıcaklıkları çizelim
plot(data$date, data$mean_T, type = "l", col = "lightblue", 
     xlab = "Tarih", ylab = "Sıcaklık (°C)", 
     main = "2017 Yılı Gunluk Sicakliklari")

# Günlük sıcaklıkların histogramını çizelim - hangi sicaklik ne kadar tekrarlanmis
hist(data$mean_T, col = "pink", 
     xlab = "Sıcaklık (°C)", ylab = "Frekans", 
     main = "2017 Yılı Gunluk Sicaklik Dağılımı", 
     breaks = 15)  # Bin sayısını ayarlayabilirsiniz


