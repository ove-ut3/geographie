departement <- readr::read_csv2("data-raw/departement.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ","))

devtools::use_data(departement, overwrite = TRUE)
