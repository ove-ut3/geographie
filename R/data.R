#' generer_data
#'
#' @export
#' @keywords internal
generer_data <- function() {

  ptt <- impexp::access_import("PTT", paste0(find.package("geographie"), "/extdata/Tables_ref.accdb")) %>%
    dplyr::rename(code_commune = com_code)
  save("ptt", file = "data/ptt.RData")

  ods_geo <- readr::read_csv2("https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-referentiel-geographique/download/?format=csv", locale = readr::locale(decimal_mark = ","),
                                   col_types = readr::cols(reg_code = readr::col_character(),
                                                           reg_code_old = readr::col_character())) %>%
    patchr::normalise_colnames() %>%
    dplyr::rename(code_commune = com_code, lib_commune = com_nom, code_uu = uu_code, lib_uu = uucr_nom, code_departement = dep_code, lib_departement = dep_nom, code_region = reg_code, lib_region = reg_nom, code_region_2015 = reg_code_old, lib_region_2015 = reg_nom_old)
  save("ods_geo", file = "data/ods_geo.RData")

  departement <- impexp::access_import("Departement", paste0(find.package("geographie"), "/extdata/Tables_ref.accdb"))
  save("departement", file = "data/departement.RData")

  region <- impexp::access_import("Region", paste0(find.package("geographie"), "/extdata/Tables_ref.accdb"))
  save("region", file = "data/region.RData")

  pays <- impexp::access_import("Pays", paste0(find.package("geographie"), "/extdata/Tables_ref.accdb"))
  save("pays", file = "data/pays.RData")

  type_voie <- impexp::access_import("Adresse_voie_type", paste0(find.package("geographie"), "/extdata/Tables_ref.accdb"))
  save("type_voie", file = "data/type_voie.RData")

  adresse_voie_prx <- impexp::access_import("Adresse_voie_prx", paste0(find.package("geographie"), "/extdata/Tables_ref.accdb"))
  save("adresse_voie_prx", file = "data/adresse_voie_prx.RData")

  cp_ville_commune <- dplyr::select(ptt, code_postal, lib_commune1 = nom_localite, code_commune) %>%
    dplyr::full_join(dplyr::select(ods_geo, code_commune, lib_commune2 = com_nom_maj, lib_commune3 = com_nom_maj_court), by = "code_commune") %>%
    dplyr::mutate(lib_commune1 = caractr::str_remove_punct(lib_commune1) %>% caractr::str_remove_accent(),
                  lib_commune2 = caractr::str_remove_punct(lib_commune2) %>% caractr::str_remove_accent(),
                  lib_commune3 = caractr::str_remove_punct(lib_commune3) %>% caractr::str_remove_accent()) %>%
    tidyr::gather(key = "champ", value = "lib_commune", dplyr::starts_with("lib_commune")) %>%
    dplyr::select(code_postal, lib_commune, code_commune) %>%
    dplyr::filter(!is.na(code_postal) & !is.na(lib_commune)) %>%
    unique()
  save("cp_ville_commune", file = "data/cp_ville_commune.RData")

}
