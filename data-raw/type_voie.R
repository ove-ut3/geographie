type_voie <- readr::read_csv2("data-raw/type_voie.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ","))

devtools::use_data(type_voie, overwrite = TRUE)
