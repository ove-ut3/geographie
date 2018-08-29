pays <- readr::read_csv2("data-raw/pays.csv", col_types = readr::cols(code_pays = "c"), locale = readr::locale(decimal_mark = ","))

devtools::use_data(pays, overwrite = TRUE)
