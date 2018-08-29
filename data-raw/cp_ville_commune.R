cp_ville_commune <- dplyr::select(geographie::ptt, code_postal, lib_commune1 = nom_localite, code_commune) %>%
  dplyr::full_join(dplyr::select(geographie::ods_geo, code_commune, lib_commune2 = com_nom_maj, lib_commune3 = com_nom_maj_court), by = "code_commune") %>%
  dplyr::mutate(lib_commune1 = caractr::str_remove_punct(lib_commune1) %>% caractr::str_remove_accent(),
                lib_commune2 = caractr::str_remove_punct(lib_commune2) %>% caractr::str_remove_accent(),
                lib_commune3 = caractr::str_remove_punct(lib_commune3) %>% caractr::str_remove_accent()) %>%
  tidyr::gather(key = "champ", value = "lib_commune", dplyr::starts_with("lib_commune")) %>%
  dplyr::select(code_postal, lib_commune, code_commune) %>%
  dplyr::filter(!is.na(code_postal) & !is.na(lib_commune)) %>%
  unique()

devtools::use_data(cp_ville_commune, overwrite = TRUE)
