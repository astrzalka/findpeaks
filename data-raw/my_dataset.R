## code to prepare `my_dataset` dataset goes here

dane_1 <- read.table('data-raw/dep_5.txt', header = FALSE)

usethis::use_data(dane_1, overwrite = TRUE)
