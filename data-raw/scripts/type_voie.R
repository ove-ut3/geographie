type_voie <- readr::read_csv2("data-raw/data/type_voie.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ",")) %>%
  dplyr::as_tibble()

usethis::use_data(type_voie, overwrite = TRUE)
