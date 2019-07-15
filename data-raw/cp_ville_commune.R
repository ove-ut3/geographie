cp_ville_commune <- dplyr::select(geographie::ptt, code_postal, lib_commune1 = nom_localite, code_commune) %>%
  dplyr::full_join(dplyr::select(geographie::ods_geo, code_commune, lib_commune2 = com_nom_maj, lib_commune3 = com_nom_maj_court), by = "code_commune") %>%
  dplyr::mutate(lib_commune1 = lib_commune1 %>%
                  stringr::str_remove_all("[[:punct:]]+", " ") %>%
                  stringi::stri_trans_general("latin-ascii"),
                lib_commune2 = lib_commune2 %>%
                  stringr::str_remove_all("[[:punct:]]+", " ") %>%
                  stringi::stri_trans_general("latin-ascii"),
                lib_commune3 = lib_commune3 %>%
                  stringr::str_remove_all("[[:punct:]]+", " ") %>%
                  stringi::stri_trans_general("latin-ascii")) %>%
  tidyr::gather(key = "champ", value = "lib_commune", dplyr::starts_with("lib_commune")) %>%
  dplyr::select(code_postal, lib_commune, code_commune) %>%
  dplyr::filter(!is.na(code_postal) & !is.na(lib_commune)) %>%
  unique()

usethis::use_data(cp_ville_commune, overwrite = TRUE)
