library(httr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Ambil Dataset melalui API
resp <- GET ("https://data.covid19.go.id/public/api/update.json")

# cek status kode
resp$status_code
identical(resp$status_code, status_code(resp))

# cek headers
headers(resp)

# mengekstrak isi respon
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE) 
length(cov_id_raw)
names(cov_id_raw)
cov_id_update <- cov_id_raw$update

# ANALISA DATA -------------------------------------------------------

# Kapan tanggal pembaharuan data penambahan kasus?
cov_id_update$penambahan$tanggal

# Berapa jumlah penambahan kasus sembuh?
cov_id_update$penambahan$jumlah_sembuh

# Berapa jumlah penambahan kasus meninggal?
cov_id_update$penambahan$jumlah_meninggal

# Berapa jumlah total kasus positif hingga saat ini?
cov_id_update$total$jumlah_positif

# Berapa jumlah total kasus meninggal hingga saat ini?
cov_id_update$total$jumlah_meninggal

# Bagaimana dengan Jawa Timur?
resp_jatim <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json")
cov_jatim_raw <- content(resp_jatim, as = "parsed", simplifyVector = TRUE)
names(cov_jatim_raw)

# Berapa jumlah total kasus covid19 di Jawa Timur?
cov_jatim_raw$kasus_total

# Persentase meninggal dunia akibat covid19 di Jawa Timur?
cov_jatim_raw$meninggal_persen

# Berapa persentase tingkat kesembuhan di Jawa Timur?
cov_jatim_raw$sembuh_persen

# Data Perkembangan Kasus Covid di Jawa Timur
cov_jatim <- cov_jatim_raw$list_perkembangan
str(cov_jatim)
head(cov_jatim)

# Data Cleansing
new_cov_jatim <-
  cov_jatim %>%
  select(-contains("DIRAWAT_OR_ISOLASI")) %>%
  select(-starts_with("AKUMULASI")) %>%
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>%
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(new_cov_jatim)

# Visualisasi kasus baru di Jatim
ggplot(new_cov_jatim, aes(x = tanggal, y = kasus_baru)) +
  geom_col()

# Visualisasi Kasus harian positif COVID-19 di Jawa Timur
ggplot(new_cov_jatim, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  )

# Grafik Kasus Sembuh Jawa Timur
ggplot(new_cov_jatim, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  )

# Grafik Kasus Meninggal Jawa Timur
ggplot(new_cov_jatim, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  )

# Melihat data pekanan
cov_jatim_pekanan <- new_cov_jatim %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

glimpse(cov_jatim_pekanan)

# Apakah pekan ini lebih baik daripada pekan lalu?
cov_jatim_pekanan <-
  cov_jatim_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(cov_jatim_pekanan)

# Bar Chart pekanan di Jawa Timur
ggplot(cov_jatim_pekanan[cov_jatim_pekanan$tahun==2020,], aes(pekan_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 9:29, expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  )

# Hingga saat ini ada berapa kasus yang masih aktif?
cov_jatim_akumulasi <- 
  new_cov_jatim %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

tail(cov_jatim_akumulasi)

# Line Chart Kasus Aktif di Jawa Timur
ggplot(data = cov_jatim_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line()

# Data Transform
dim(cov_jatim_akumulasi)

cov_jatim_akumulasi_pivot <- 
  cov_jatim_akumulasi %>% 
  pivot_longer(
    cols = -tanggal,
    names_to = "kategori",
    names_prefix = "akumulasi_",
    values_to = "jumlah"
  )

dim(cov_jatim_akumulasi_pivot)

glimpse(cov_jatim_akumulasi_pivot)

# Grafik Perbandingan antara Akumulasi Kasus Aktif, Kasus Sembuh, dan Kasus Meninggal
ggplot(cov_jatim_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
