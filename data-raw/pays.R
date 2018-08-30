pays <- readr::read_csv2("data-raw/pays.csv", col_types = readr::cols(code_pays = "c"), locale = readr::locale(decimal_mark = ","))

devtools::use_data(pays, overwrite = TRUE)

pays_libelle <- readr::read_csv2("data-raw/pays_libelle.csv", col_types = readr::cols(code_pays = "c"), locale = readr::locale(decimal_mark = ","))

devtools::use_data(pays_libelle, overwrite = TRUE)
