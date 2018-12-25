adresse_voie_prx <- readr::read_csv2("data-raw/adresse_voie_prx.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ","))

usethis::use_data(adresse_voie_prx, overwrite = TRUE)
