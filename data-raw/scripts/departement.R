departement <- readr::read_csv2("data-raw/data/departement.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ",")) %>%
  dplyr::as_tibble()

usethis::use_data(departement, overwrite = TRUE)
