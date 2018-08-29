ods_geo <- readr::read_csv2("https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-referentiel-geographique/download/?format=csv", locale = readr::locale(decimal_mark = ","),
                            col_types = readr::cols(reg_code = readr::col_character(),
                                                    reg_code_old = readr::col_character())) %>%
  patchr::normalise_colnames() %>%
  dplyr::rename(code_commune = com_code, lib_commune = com_nom, code_uu = uu_code, lib_uu = uucr_nom, code_departement = dep_code, lib_departement = dep_nom, code_region = reg_code, lib_region = reg_nom, code_region_2015 = reg_code_old, lib_region_2015 = reg_nom_old)

devtools::use_data(ods_geo, overwrite = TRUE)
