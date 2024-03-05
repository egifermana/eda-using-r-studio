# Case Study: Customer Satisfaction, R Studio

## Case Study
* Seorang Marketing Manager sebuah perusahaan ingin mengetahui profil demografi konsumen pengguna mereknya, karena itu dilakukan survei terhadap 300 responden pengguna mereknya yang tersebar di 6 kota. Keluarkan output demografi (kota, usia, jenis kelamin, pendidikan dan status perkawinan).
* Tingkat kepuasan konsumen terhadap Brand A pada data, diukur dari tingkat kepuasan dalam hal pelayanan, kualitas produk dan harga. Lakukan uji validitas dan reliabilitas pada data, apakah variabel tingkat kepuasan dalam hal pelayanan, kualitas produk dan harga dapat digunakan untuk mengukur tingkat kepuasan secara keseluruhan dengan baik! (alpha = 0.6).
* Marketing Manager tersebut juga ingin mengetahui tingkat kepuasan konsumennya di masing-masing kota. Ada 4 tingkat kepuasan yang diukur yaitu tingkat kepuasan konsumen secara keseluruhan, tingkat kepuasan konsumen terhadap pelayanan yang diberikan, tingkat kepuasan konsumen terhadap kualitas produk dan tingkat kepuasan konsumen terhadap harga produk. Tingkat kepuasan didefinisikan sebagai rata-rata dari tingkat kepuasan semua responden.
* Keluarkan tingkat kepuasan konsumen secara keseluruhan untuk masing-masing kota dan secara total (gabungan semua kota).
* Apakah ada perbedaan tingkat kepuasan untuk masing-masing kota? Jika ada, kota mana yang berbeda? Analisa pada alpha = 5.0.

## R Syntax
Read CSV File
```R
data <- read.csv('data.csv, header = TRUE)
```
Mean dan standar deviasi dari demografi
```R
demographic <- aggregate(cbind(usia, sex, didik, status) ~ kota, data = data,
               FUN = function(x) c(mean = mean(x), sd = sd(x)))
print(demographic)
```
Frekuensi demografi kota, usia, sex, pendidikan, dan status pernikahan
```R
kota_freq <- table(data$kota)
usia_freq <- table(data$usia)
sex_freq <- table(data$sex)
didik_freq <- table(data$didik)
status_freq <- table(data$status)

print("Frekuensi Kota:")
print(kota_freq)
print("Frekuensi Usia:")
print(usia_freq)
print("Frekuensi Jenis Kelamin:")
print(sex_freq)
print("Frekuensi Pendidikan:")
print(didik_freq)
print("Frekuensi Status:")
print(status_freq)
```
Uji validitas dan reliabilitas, dengan alpha = 0.6
```R
library(psych)

satisfaction <- data[, c("puas_lay", "puas_kua", "puas_pri")]
reliability_result <- alpha(satisfaction)
print(reliability_result)

if(all(reliability_result$total > 0.6)) {
  print("Reliabilitas dapat diterima.")
} else {
  print("Reliabilitas tidak dapat diterima.")
}
```
Tingkat kepuasan konsumen masing-masing kota
```R
total_satisfaction_by_city <- aggregate(cbind(puas_all) ~ kota, 
                                        data = data, FUN = sum)
total_overall_satisfaction <- mean(data$puas_all)
print(total_satisfaction_by_city)
print(paste("Total kepuasan konsumen keseluruhan: ", 
            total_overall_satisfaction))
```
Test apakah ada perbedaan tingkat kepuasan masing-masing kota, dengan alpha = 5.0
```R
library(stats)

data$kota <- as.factor(data$kota)
anova_result <- aov(puas_all ~ kota, data = data)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
significant_differences <- tukey_result$`kota`[, 4]
print("Perbedaan signifikan:")
print(significant_differences)
```
