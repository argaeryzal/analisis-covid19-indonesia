# Analisis Data Kasus Covid-19 di Indonesia
Pada kesempatan kali ini, saya akan melakukan analisis data kasus covid-19 di Indonesia khususnya untuk Provinsi Jawa Timur disertai dengan visualisasi yang akan menjelaskan secara detail dan menjawab berbagai permasalahan menggunakan bahasa pemograman R.

Langkah pertama yang akan kita lakukan adalah mengimport library yang dibutuhkan untuk menganalisis dan visualisasi grafik.
````
library(httr)
library(dplyr)
library(tidyverse)
library(ggplot2)
````

## Data Preparation
Next kita akan mempersiapkan data yang ingin digunakan hingga dapat dianalisa dengan baik. Saya akan mengambil dataset dari [Data COVID-19 Indonesia](https://data.covid19.go.id/public/api/update.json).
````````````
# Ambil Dataset melalui API
> resp <- GET ("https://data.covid19.go.id/public/api/update.json")

# cek status kode
> resp$status_code
> identical(resp$status_code, status_code(resp))

# cek headers
> headers(resp)

# mengekstrak isi respon
> cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE) 
> length(cov_id_raw)
> names(cov_id_raw)
> cov_id_update <- cov_id_raw$update
````````````

## Analisis Data
Selanjutnya kita akan melakukan analisa awal data yang diperoleh dari sumber covid.19.go.id dengan menjawab pertanyaan - pertanyaan dibawah ini.
### Kapan tanggal pembaharuan data penambahan kasus?
```
> cov_id_update$penambahan$tanggal
[1] "2021-10-03"
```
### Berapa jumlah penambahan kasus sembuh?
```
> cov_id_update$penambahan$jumlah_sembuh
[1] 2020
```
### Berapa jumlah penambahan kasus meninggal?
```
> cov_id_update$penambahan$jumlah_meninggal
[1] 58
```
### Berapa jumlah total kasus positif hingga saat ini?
```
> cov_id_update$total$jumlah_positif
[1] 4219284
```
### Berapa jumlah total kasus meninggal hingga saat ini?
```
> cov_id_update$total$jumlah_meninggal
[1] 142173
```
Pertanyaan selanjutnya adalah **Bagaimana dengan kasus di Provinsi Jawa Timur?**
Untuk menjawab itu semua, pertama kita akan mengambil dataset berformat JSON dari [Data COVID Provinsi Jawa Timur](https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json).
```
> resp_jatim <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json")
> cov_jatim_raw <- content(resp_jatim, as = "parsed", simplifyVector = TRUE)
> names(cov_jatim_raw)
 [1] "last_date"            "provinsi"             "kasus_total"          "kasus_tanpa_tgl"     
 [5] "kasus_dengan_tgl"     "meninggal_persen"     "meninggal_tanpa_tgl"  "meninggal_dengan_tgl"
 [9] "sembuh_persen"        "sembuh_tanpa_tgl"     "sembuh_dengan_tgl"    "list_perkembangan"   
[13] "data"
```
### Berapa jumlah total kasus covid19 di Jawa Timur?
```
> cov_jatim_raw$kasus_total
[1] 395828
```
### Berapa persentase meninggal dunia akibat covid19 di Jawa Timur?
```
> cov_jatim_raw$meninggal_persen
[1] 7.426205
```
### Berapa persentase tingkat kesembuhan di Jawa Timur?
```
> cov_jatim_raw$sembuh_persen
[1] 91.88865
```
## Data Perkembangan Kasus COVID-19 di Jawa Timur
```
> cov_jatim <- cov_jatim_raw$list_perkembangan
> str(cov_jatim)
'data.frame':	565 obs. of  9 variables:
 $ tanggal                     : num  1.58e+12 1.58e+12 1.58e+12 1.58e+12 1.58e+12 ...
 $ KASUS                       : int  7 3 9 9 14 2 3 9 13 3 ...
 $ MENINGGAL                   : int  1 0 0 1 0 1 1 0 1 1 ...
 $ SEMBUH                      : int  0 0 0 0 0 0 0 1 0 0 ...
 $ DIRAWAT_OR_ISOLASI          : int  6 3 9 8 14 1 2 8 12 2 ...
 $ AKUMULASI_KASUS             : int  7 10 19 28 42 44 47 56 69 72 ...
 $ AKUMULASI_SEMBUH            : int  0 0 0 0 0 0 0 1 1 1 ...
 $ AKUMULASI_MENINGGAL         : int  1 1 1 2 2 3 4 4 5 6 ...
 $ AKUMULASI_DIRAWAT_OR_ISOLASI: int  6 9 18 26 40 41 43 51 63 65 ...
> head(cov_jatim)
       tanggal KASUS MENINGGAL SEMBUH DIRAWAT_OR_ISOLASI AKUMULASI_KASUS AKUMULASI_SEMBUH
1 1.584490e+12     7         1      0                  6               7                0
2 1.584576e+12     3         0      0                  3              10                0
3 1.584662e+12     9         0      0                  9              19                0
4 1.584749e+12     9         1      0                  8              28                0
5 1.584835e+12    14         0      0                 14              42                0
6 1.584922e+12     2         1      0                  1              44                0
  AKUMULASI_MENINGGAL AKUMULASI_DIRAWAT_OR_ISOLASI
1                   1                            6
2                   1                            9
3                   1                           18
4                   2                           26
5                   2                           40
6                   3                           41
```
## Data Wrangling / Cleansing
```
> new_cov_jatim <-
+   cov_jatim %>%
+   select(-contains("DIRAWAT_OR_ISOLASI")) %>%
+   select(-starts_with("AKUMULASI")) %>%
+   rename(
+     kasus_baru = KASUS,
+     meninggal = MENINGGAL,
+     sembuh = SEMBUH
+   ) %>%
+   mutate(
+     tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
+     tanggal = as.Date(tanggal)
+   )
> str(new_cov_jatim)
'data.frame':	565 obs. of  4 variables:
 $ tanggal   : Date, format: "2020-03-18" "2020-03-19" "2020-03-20" ...
 $ kasus_baru: int  7 3 9 9 14 2 3 9 13 3 ...
 $ meninggal : int  1 0 0 1 0 1 1 0 1 1 ...
 $ sembuh    : int  0 0 0 0 0 0 0 1 0 0 ...
 ```
 ## Visualisasi Data
 Kita akan memvisualisasikan data yang telah disajikan diatas sehingga dapat dilihat dan dipahami dengan mudah oleh siapa pun.
 ### Bagaimana Grafik Kasus Harian Positif COVID-19 di Jawa Timur?
  ![kasus_harian_positif_jatim](https://user-images.githubusercontent.com/20991856/135788030-37a9b64a-879b-423d-b036-bbec5d287f8d.png)
### Bagaimana Grafik Kasus Harian Sembuh di Jawa Timur?
![kasus_harian_sembuh_jatim](https://user-images.githubusercontent.com/20991856/135788223-45039c72-9219-4141-ae1d-b53d1060fe2f.png)
### Bagaimana Grafik Kasus Harian Meninggal di Jawa Timur?
 ![kasus_harian_meninggal_jatim](https://user-images.githubusercontent.com/20991856/135788336-19da7d1f-2af3-40da-9ec3-a4d90efd31d4.png)
### Melihat Data Pekanan
```
> cov_jatim_pekanan <- new_cov_jatim %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )
> glimpse(cov_jatim_pekanan)
Rows: 82
Columns: 3
$ tahun    <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,…
$ pekan_ke <dbl> 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,…
$ jumlah   <int> 47, 59, 110, 295, 140, 247, 316, 513, 716, 1533, 1144, 1288, 1701, 1726, 1990, 2415…
```
Dari data diatas bisa dilihat, mulai dari pekan ke 12 di tahun 2020 kasus covid-19 di jawa timur mengalami kelonjakan yang pesat, dilihat dari data jumlah pekan ke pekan.
### Apakah Pekan ini lebih baik dari Pekan sebelumnya?
```
> cov_jatim_pekanan <-
  cov_jatim_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
> glimpse(cov_jatim_pekanan)
Rows: 82
Columns: 5
$ tahun            <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 202…
$ pekan_ke         <dbl> 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,…
$ jumlah           <int> 47, 59, 110, 295, 140, 247, 316, 513, 716, 1533, 1144, 1288, 1701, 1726, 19…
$ jumlah_pekanlalu <dbl> 0, 47, 59, 110, 295, 140, 247, 316, 513, 716, 1533, 1144, 1288, 1701, 1726,…
$ lebih_baik       <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, …
```
Bisa dilihat data diatas, data pekan ke 12 tahun 2020 terjadi penambahan jumlah sebanyak 47 kasus yang mana pekan lalu terjadi 0 kasus yang artinya pekan ke 12 tidak lebih baik dari pekan sebelumnya.

### Bar Chart Data Pekanan Positif Jawa Timur
![kasus_pekanan_positif_jatim](https://user-images.githubusercontent.com/20991856/135789029-bf74210d-e6ab-4e41-941e-399b9d0391eb.png)
### Hingga saat ini ada berapa kasus yang masih aktif?
```
> cov_jatim_akumulasi <- 
  new_cov_jatim %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
> tail(cov_jatim_akumulasi)
       tanggal akumulasi_aktif akumulasi_sembuh akumulasi_meninggal
560 2021-09-28            2390           363292               29350
561 2021-09-29            2565           363339               29352
562 2021-09-30            2713           363384               29354
563 2021-10-01            2784           363435               29356
564 2021-10-02            2865           363480               29359
565 2021-10-03            2936           363520               29363
```
Bisa dilihat data tanggal 03 Oktober 2021 dimana akumulasi aktif sebanyak 2936, akumulasi sembuh sebanyak 363520, akumulasi meninggal sebanyak 29363. Ini merupakan sebuah kemajuan bagi penanganan covid di Indonesia dikarenakan akumulasi jumlah kasus aktif di Provinsi Jawa Timur saat ini hanya tersisa 2936.

### Line Chart Kasus Aktif di Jawa Timur
![kasus_aktif_jatim_line_chart](https://user-images.githubusercontent.com/20991856/135789361-5f9ef528-506b-454b-b3d1-51ecff4a068d.png)

### Pivot Data Akumulasi Kasus COVID-19 di Jawa Timur
```
> dim(cov_jatim_akumulasi)
[1] 565   4
> cov_jatim_akumulasi_pivot <- 
    cov_jatim_akumulasi %>% 
    pivot_longer(
      cols = -tanggal,
      names_to = "kategori",
      names_prefix = "akumulasi_",
      values_to = "jumlah"
    )
> dim(cov_jatim_akumulasi_pivot)
[1] 1695    3
> glimpse(cov_jatim_akumulasi_pivot)
Rows: 1,695
Columns: 3
$ tanggal  <date> 2020-03-18, 2020-03-18, 2020-03-18, 2020-03-19, 2020-03-19, 2020-03-19, 2020-03-20…
$ kategori <chr> "aktif", "sembuh", "meninggal", "aktif", "sembuh", "meninggal", "aktif", "sembuh", …
$ jumlah   <int> 6, 0, 1, 9, 0, 1, 18, 0, 1, 26, 0, 2, 40, 0, 2, 41, 0, 3, 43, 0, 4, 51, 1, 4, 63, 1…
```
### Grafik Perbandingan Kasus Aktif, Meninggal, Sembuh
![grafik_perbandingan_kasus_covid_jatim](https://user-images.githubusercontent.com/20991856/135789627-9f663346-96a8-488b-80fd-a0cd8b2eecfc.png)

Dilihat dari grafik perbandingan kasus aktif, meninggal, dan sembuh di Provinsi Jawa Timur diatas, dapat kita simpulkan :
- Peningkatan kasus aktif tertinggi terjadi pada bulan Agustus dan terus melandai hingga sekarang ini
- Kenaikan jumlah sembuh tertinggi berada pada antara bulan September, Oktober dan terus meningkat sejak awal kemunculan covid-19 di Indonesia
- Sedangkan data kasus meinggal terus meningkat sejak awal pandemi covid-19 melanda Indonesia hingga sekarang ini adalah fase tertinggi kasus meninggal dunia di Provinsi Jawa Timur

Data tersebut bersumber dari : Covid.19.go.id

Analisis Kasus COVID-19 di Provinsi Jawa Timur dilakukan pada tanggal 04 Oktober 2021 dengan update data paling mutakhir ada pada tanggal 03 Oktober 2021. Apabila ingin mendapatkan data yang lebih terbaru, harap melakukan analisis ulang menggunakan source code yang telah saya sediakan di Github.

*Salam Hangat,* 

*Arga Eryzal Pradinata*
