adresse_voie_prx <- readr::read_csv2("data-raw/data/adresse_voie_prx.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ",")) %>%
  dplyr::as_tibble()

usethis::use_data(adresse_voie_prx, overwrite = TRUE)
