ptt <- readr::read_csv2("data-raw/ptt.csv", col_types = paste0(c(rep("c", 9), rep("d", 3), rep("c", 2)), collapse = ""), locale = readr::locale(decimal_mark = ","))

devtools::use_data(ptt, overwrite = TRUE)
