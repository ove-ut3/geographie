region <- readr::read_csv2("data-raw/region.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ","))

usethis::use_data(region, overwrite = TRUE)
