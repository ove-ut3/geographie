pays <- readr::read_csv2("data-raw/data/pays.csv", col_types = readr::cols(code_pays = "c"), locale = readr::locale(decimal_mark = ","))

usethis::use_data(pays, overwrite = TRUE)

pays_libelle <- readr::read_csv2("data-raw/data/pays_libelle.csv", col_types = readr::cols(code_pays = "c"), locale = readr::locale(decimal_mark = ",")) %>%
  dplyr::as_tibble()

usethis::use_data(pays_libelle, overwrite = TRUE)
