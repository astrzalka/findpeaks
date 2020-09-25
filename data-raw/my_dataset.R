## code to prepare `my_dataset` dataset goes here

dane_1 <- read.table('data-raw/dep_5.txt', header = FALSE)

usethis::use_data(dane_1, overwrite = TRUE)

dane_2 <- read.table('~/findpeaks_test/imagej_test/Valuesfrosk01_07_R3D-2.tif_C1.txt')

colnames(dane_2) <- c('V1', 'V2')

usethis::use_data(dane_2, overwrite = TRUE)
