#' Obtenir le libelle de commune a partir d'un code commune
#'
#' Obtenir le libellé de commune à partir d'un code commune.
#'
#' @param code_commune Un vecteur de code commune.
#'
#' @return Un vecteur de libellé de commune.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::lib_commune(c("01001", "33003"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' data <- dplyr::tibble(code_commune = c("01001", "33003"))
#' dplyr::mutate(data, libelle = geographie::lib_commune(code_commune))
#'
#' @export
lib_commune <- function(code_commune) {

  if (class(code_commune) != "character") {
    stop("Le code commune doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_commune)) %>% length() == 0) {
    return(code_commune)
  }

  test_longueur <- purrr::map_int(code_commune, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code commune n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  lib_commune <- dplyr::tibble(code_commune) %>%
    dplyr::left_join(dplyr::select(geographie::ods_geo, code_commune, lib_commune), by = "code_commune") %>%
    dplyr::pull(lib_commune)

  return(lib_commune)
}

#' Obtenir le libelle d'unite urbaine a partir d'un code d'unite urbaine
#'
#' Obtenir le libellé d'unité urbaine à partir d'un code d'unité urbaine.
#'
#' @param code_uu Un vecteur de code d'unité urbaine.
#'
#' @return Un vecteur de libellé d'unité urbaine.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::lib_uu(c("01302", "33701"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' dplyr::tibble(code_uu = c("01302", "33701")) %>%
#'   dplyr::mutate(libelle = geographie::lib_uu(code_uu))
#'
#' @export
lib_uu <- function(code_uu) {

  if (class(code_uu) != "character") {
    stop("Le code d'unité urbaine doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_uu)) %>% length() == 0) {
    return(code_uu)
  }

  test_longueur <- purrr::map_int(code_uu, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code d'unité urbaine n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  lib_uu <- dplyr::select(geographie::ods_geo, code_uu, lib_uu) %>%
    tidyr::drop_na(code_uu) %>%
    unique() %>%
    dplyr::left_join(dplyr::tibble(code_uu), ., by = "code_uu") %>%
    dplyr::pull(lib_uu)

  return(lib_uu)
}

#' Obtenir le libelle de pays a partir d'un code pays (code INSEE)
#'
#' Obtenir le libellé de pays à partir d'un code pays (code INSEE).
#'
#' @param code_pays Un vecteur de code pays (code INSEE).
#' @param langue Un code langue (\code{fr} ou \code{en}).
#'
#' @return Un vecteur de libellé de pays.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::lib_pays(c("100", "109"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' dplyr::tibble(code_pays = c("100", "109")) %>%
#'   dplyr::mutate(libelle = geographie::lib_pays(code_pays),
#'                 libelle_en = geographie::lib_pays(code_pays, langue = "en"))
#'
#' @export
lib_pays <- function(code_pays, langue = "fr") {

  if (class(code_pays) != "character") {
    stop("Le code pays doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_pays)) %>% length() == 0) {
    return(code_pays)
  }

  if (!langue %in% c("fr", "en")) {
    stop("La langue du libellé doit être \"fr\" ou \"en\"", call. = FALSE)
  }

  test_longueur <- purrr::map_int(code_pays, nchar) %in% c(3, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code pays n'est pas de longueur 3: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  if (langue == "fr") champ_lib_pays <- "lib_pays_fr"
  else if (langue == "en") champ_lib_pays <- "lib_pays_en"

  lib_pays <- dplyr::select(geographie::pays, code_pays, lib_pays_fr, lib_pays_en) %>%
    tidyr::drop_na(code_pays) %>%
    dplyr::left_join(dplyr::tibble(code_pays), ., by = "code_pays") %>%
    .[[champ_lib_pays]]

  return(lib_pays)
}

#' Obtenir le libelle de pays a partir d'un code pays (code INSEE)
#'
#' Obtenir le libellé de pays à partir d'un code pays (code INSEE).
#'
#' @param code_pays Un vecteur de code pays (code INSEE).
#' @param langue Un code langue (\code{fr} ou \code{en}).
#'
#' @return Un vecteur de libellé de pays.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::lib_pays_eu(c("FR", "DE"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' dplyr::tibble(code_pays = c("FR", "DE")) %>%
#'   dplyr::mutate(libelle = geographie::lib_pays_eu(code_pays),
#'                 libelle_en = geographie::lib_pays_eu(code_pays, langue = "en"))
#'
#' @export
lib_pays_eu <- function(code_pays_eu, langue = "fr") {

  if (class(code_pays_eu) != "character") {
    stop("Le code pays doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_pays_eu)) %>% length() == 0) {
    return(code_pays_eu)
  }

  if (!langue %in% c("fr", "en")) {
    stop("La langue du libellé doit être \"fr\" ou \"en\"", call. = FALSE)
  }

  test_longueur <- purrr::map_int(code_pays_eu, nchar) %in% c(2, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code pays n'est pas de longueur 2: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  if (langue == "fr") champ_lib_pays <- "lib_pays_fr"
  else if (langue == "en") champ_lib_pays <- "lib_pays_en"

  lib_pays_eu <- dplyr::select(geographie::pays, code_pays_eu = code_pays_iso2, lib_pays_fr, lib_pays_en) %>%
    tidyr::drop_na(code_pays_eu) %>%
    dplyr::left_join(dplyr::tibble(code_pays_eu), ., by = "code_pays_eu") %>%
    .[[champ_lib_pays]]

  return(lib_pays_eu)
}

#' Obtenir le libelle de type de voie a partir du code
#'
#' Obtenir le libellé de type de voie à partir du code.
#'
#' @param code_type_voie Un vecteur de code de type de voie.
#'
#' @return Un vecteur de libellé de type de voie.
#'
#' Jeu de données source : \code{geographie::type_voie}.\cr
#'
#' @examples
#' geographie::lib_type_voie(c("AV", "BD", "QUA"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' dplyr::tibble(code_type_voie = c("AV", "BD", "QUA")) %>%
#'   dplyr::mutate(libelle = geographie::lib_type_voie(code_type_voie))
#'
#' @export
lib_type_voie <- function(code_type_voie) {

  if (class(code_type_voie) != "character") {
    stop("Le code de type de voie doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_type_voie)) %>% length() == 0) {
    return(code_type_voie)
  }

  lib_type_voie <- dplyr::tibble(code_type_voie) %>%
    dplyr::left_join(dplyr::select(geographie::type_voie, code_type_voie, lib_type_voie), by = "code_type_voie") %>%
    dplyr::pull(lib_type_voie)

  return(lib_type_voie)
}

#' Obtenir le libelle de departement a partir du code departement
#'
#' Obtenir le libellé de département à partir du code département.
#'
#' @param code_departement Un vecteur de code de département.
#'
#' @return Un vecteur de libellé de département.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::lib_departement(c("01", "33"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' data <- dplyr::tibble(code_departement = c("01", "33"))
#' data <- dplyr::mutate(data, libelle = geographie::lib_departement(code_departement))
#'
#' @export
lib_departement <- function(code_departement) {

  if (class(code_departement) != "character") {
    stop("Le code département doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_departement)) %>% length() == 0) {
    return(code_departement)
  }

  test_longueur <- purrr::map_int(code_departement, nchar) %in% c(2, 3, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code département n'est pas de longueur 2 ou 3: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  lib_departement <- dplyr::tibble(code_departement) %>%
    dplyr::left_join(dplyr::select(geographie::ods_geo, code_departement, lib_departement) %>%
                       dplyr::mutate(code_departement = stringr::str_pad(code_departement, 3, "left", "0")) %>%
                       unique(),
                     by = "code_departement") %>%
    dplyr::pull(lib_departement)

  return(lib_departement)
}

#' Obtenir le libelle de region a partir du code region
#'
#' Obtenir le libellé de région à partir du code région.
#'
#' @param code_region Un vecteur de code de région.
#'
#' @return Un vecteur de libellé de région.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::lib_region(c("84", "75"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' dplyr::tibble(code_region = c("84", "75")) %>%
#'   dplyr::mutate(libelle = geographie::lib_region(code_region))
#'
#' @export
lib_region <- function(code_region) {

  if (class(code_region) != "character") {
    stop("Le code région doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_region)) %>% length() == 0) {
    return(code_region)
  }

  test_longueur <- purrr::map_int(code_region, nchar) %in% c(1, 2, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code région n'est pas de longueur 1 ou 2: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  lib_region <- dplyr::tibble(code_region) %>%
    dplyr::left_join(geographie::region, by = "code_region") %>%
    dplyr::pull(lib_region)

  return(lib_region)
}

#' Obtenir le libelle de region (2015 et avant) a partir du code region
#'
#' Obtenir le libellé de région (2015 et avant) à partir du code région.
#'
#' @param code_region_2015 Un vecteur de code de région (2015 et avant).
#'
#' @return Un vecteur de libellé de région (2015 et avant).
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::lib_region_2015(c("82", "72"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' dplyr::tibble(code_region_2015 = c("82", "72")) %>%
#'   dplyr::mutate(libelle = geographie::lib_region_2015(code_region_2015))
#'
#' @export
lib_region_2015 <- function(code_region_2015) {

  if (class(code_region_2015) != "character") {
    stop("Le code région doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_region_2015)) %>% length() == 0) {
    return(code_region_2015)
  }

  test_longueur <- purrr::map_int(code_region_2015, nchar) %in% c(1, 2, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code région  (2015 et avant) n'est pas de longueur 1 ou 2: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  lib_region_2015 <- dplyr::tibble(code_region_2015) %>%
    dplyr::left_join(dplyr::select(geographie::ods_geo, code_region_2015, lib_region_2015) %>% unique(),
                     by = "code_region_2015") %>%
    dplyr::pull(lib_region_2015)

  return(lib_region_2015)
}

#' Obtenir le libelle de nationalite a partir d'un code pays (code INSEE)
#'
#' Obtenir le libellé de nationalité à partir d'un code pays (code INSEE).
#'
#' @param code_pays Un vecteur de code pays (code INSEE).
#'
#' @return Un vecteur de libellé de nationalité.
#'
#' Jeu de données source : \code{geographie::pays}.\cr
#'
#' @examples
#' geographie::lib_nationalite(c("100", "109"))
#'
#' # Création d'un champ dans un data frame avec la fonction "mutate"
#' dplyr::tibble(code_pays = c("100", "109")) %>%
#'   dplyr::mutate(libelle = geographie::lib_nationalite(code_pays))
#'
#' @export
lib_nationalite <- function(code_pays) {

  if (class(code_pays) != "character") {
    stop("Le code pays doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_pays)) %>% length() == 0) {
    return(code_pays)
  }

  test_longueur <- purrr::map_int(code_pays, nchar) %in% c(3, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code pays n'est pas de longueur 3: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  lib_nationalite <- dplyr::select(geographie::pays, code_pays, lib_nationalite) %>%
    tidyr::drop_na(code_pays) %>%
    dplyr::left_join(dplyr::tibble(code_pays), ., by = "code_pays") %>%
    dplyr::pull(lib_nationalite)

  return(lib_nationalite)
}

# lib_langue
