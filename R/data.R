#' data
#'
#' @export
data <- function() {

  data_ptt <- importr::importer_table_access("PTT", paste0(racine_packages, "geographie/Tables_ref.accdb")) %>%
    dplyr::rename(code_commune = com_code)
  save("data_ptt", file = paste0(racine_packages, "geographie/data/data_ptt.RData"))

  data_ods_geo <- readr::read_csv2("https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-referentiel-geographique/download/?format=csv",
                                   col_types = readr::cols(reg_code = readr::col_character(),
                                                           reg_code_old = readr::col_character())) %>%
    importr::normaliser_nom_champs() %>%
    dplyr::rename(code_commune = com_code, lib_commune = com_nom, code_uu = uu_code, lib_uu = uucr_nom, code_departement = dep_code, lib_departement = dep_nom, code_region = reg_code, lib_region = reg_nom, code_region_2015 = reg_code_old, lib_region_2015 = reg_nom_old)
  save("data_ods_geo", file = paste0(racine_packages, "geographie/data/data_ods_geo.RData"))

  data_departement <- importr::importer_table_access("Departement", paste0(racine_packages, "geographie/Tables_ref.accdb"))
  save("data_departement", file = paste0(racine_packages, "geographie/data/data_departement.RData"))

  data_pays <- importr::importer_table_access("Pays", paste0(racine_packages, "geographie/Tables_ref.accdb"))
  save("data_pays", file = paste0(racine_packages, "geographie/data/data_pays.RData"))

  data_type_voie <- importr::importer_table_access("Adresse_voie_type", paste0(racine_packages, "geographie/Tables_ref.accdb"))
  save("data_type_voie", file = paste0(racine_packages, "geographie/data/data_type_voie.RData"))

  data_adresse_voie_prx <- importr::importer_table_access("Adresse_voie_prx", paste0(racine_packages, "geographie/Tables_ref.accdb"))
  save("data_adresse_voie_prx", file = paste0(racine_packages, "geographie/data/data_adresse_voie_prx.RData"))

  data_cp_ville_commune <- dplyr::select(data_ptt, code_postal, lib_commune1 = nom_localite, code_commune) %>%
    dplyr::full_join(select(data_ods_geo, code_commune, lib_commune2 = com_nom_maj, lib_commune3 = com_nom_maj_court), by = "code_commune") %>%
    dplyr::mutate(lib_commune1 = caractr::sans_ponctuation(lib_commune1) %>% caractr::sans_accent(),
                  lib_commune2 = caractr::sans_ponctuation(lib_commune2) %>% caractr::sans_accent(),
                  lib_commune3 = caractr::sans_ponctuation(lib_commune3) %>% caractr::sans_accent()) %>%
    tidyr::gather(key = "champ", value = "lib_commune", starts_with("lib_commune")) %>%
    dplyr::select(code_postal, lib_commune, code_commune) %>%
    dplyr::filter(!is.na(code_postal) & !is.na(lib_commune)) %>%
    unique()
  save("data_cp_ville_commune", file = paste0(racine_packages, "geographie/data/data_cp_ville_commune.RData"))

}
