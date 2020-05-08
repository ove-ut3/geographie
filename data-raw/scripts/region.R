region <- readr::read_csv2("data-raw/data/region.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ",")) %>%
  dplyr::as_tibble()

usethis::use_data(region, overwrite = TRUE)
