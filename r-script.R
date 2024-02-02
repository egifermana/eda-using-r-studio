# Read CSV file
data <- read.csv("data.csv", header=TRUE)

# No. 1, mean dan standar deviasi dari demografi
demographic <- aggregate(cbind(usia, sex, didik, status) ~ kota, data = data,
               FUN = function(x) c(mean = mean(x), sd = sd(x)))
print(demographic)

# No. 1, frekuensi demografi kota, usia, sex, pendidikan, dan status pernikahan
kota_freq <- table(data$kota)
usia_freq <- table(data$usia)
sex_freq <- table(data$sex)
didik_freq <- table(data$didik)
status_freq <- table(data$status)

# Print demografi kota, usia, sex, pendidikan, dan status pernikahan
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

# No. 2, uji validitas dan reliabilitas, denngan alpha = 0.6
library(psych)

satisfaction <- data[, c("puas_lay", "puas_kua", "puas_pri")]
reliability_result <- alpha(satisfaction)
print(reliability_result)

if(all(reliability_result$total > 0.6)) {
  print("Reliabilitas dapat diterima.")
} else {
  print("Reliabilitas tidak dapat diterima.")
}

# No. 3a, tingkat kepuasaan konsumen masing-masing kota
total_satisfaction_by_city <- aggregate(cbind(puas_all) ~ kota, 
                                        data = data, FUN = sum)
total_overall_satisfaction <- mean(data$puas_all)
print(total_satisfaction_by_city)
print(paste("Total kepuasan konsumen keseluruhan: ", 
            total_overall_satisfaction))

# No. 3b, tes apakah ada perbedaan tingkat kepuasan 
# masing-masing kota, dengan alpha = 5.0
library(stats)
data$kota <- as.factor(data$kota)
anova_result <- aov(puas_all ~ kota, data = data)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
significant_differences <- tukey_result$`kota`[, 4]
print("Perbedaan signifikan:")
print(significant_differences)
