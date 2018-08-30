#' Obtenir le code commune a partir d'un code postal
#'
#' Obtenir le code commune à partir d'un code postal.
#'
#' @param code_postal Un vecteur de codes postaux de type caractère.
#' @param bureau_distributeur Filtrer aux bureaux distributeurs uniquement.
#'
#' @return Un vecteur de code commune.\cr
#' Seuls les codes communes non-doublonnés sont retournés.
#'
#' Jeu de données source : \code{geographie::ptt}.\cr
#' Il est créé à partir de la table "N_PTT" de la BCE.
#'
#' @examples
#' # Deux exemples de codes postaux : le premier n'est lié qu'à un seul code commune mais pas le second
#' geographie::conv_cp_commune(c("01001", "01000"))
#'
#' @export
conv_cp_commune <- function(code_postal, bureau_distributeur = FALSE) {

  if (class(code_postal) != "character") {
    stop("Le code postal doit être de type character", call. = FALSE)
  }

  test_longueur <- purrr::map_int(code_postal, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    message("Au moins un code postal n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  ptt <- geographie::ptt

  if (bureau_distributeur == TRUE) {
    ptt <- tidyr::drop_na(ptt, bureau_distributeur)
  }

  conv_cp_commune <- ptt %>%
    dplyr::select(code_postal, code_commune) %>%
    dplyr::group_by(code_postal) %>%
    dplyr::filter(n() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::tibble(code_postal), ., by = "code_postal") %>%
    dplyr::pull(code_commune)

  return(conv_cp_commune)
}

#' Obtenir le code commune a partir d'un code postal et d'un libelle de commune
#'
#' Obtenir le code commune à partir d'un code postal et d'un libellé de commune.
#'
#' @param code_postal Un vecteur de codes postaux de type caractère.
#' @param lib_commune Un vecteur de libellés de commune.
#'
#' @return Un vecteur de code commune.
#'
#' Jeu de données source : \code{geographie::cp_ville_commune}.\cr
#' Il est créé à partir du jeu de données ODS Référentiel géographique français (voir projet "Géographie").
#'
#' @examples
#' geographie::conv_cp_ville_commune(code_postal = c("17000", "34010"),
#'   lib_commune = c("La Rochelle", "Montpellier"))
#'
#' @export
conv_cp_ville_commune <- function(code_postal, lib_commune) {

  if (length(code_postal) != length(code_postal)) {
    stop("Les vecteurs code_postal et lib_commune doivent être de même longueur", call. = FALSE)
  }

  if (class(code_postal) != "character") {
    stop("Le code postal doit être de type character", call. = FALSE)
  }

  if (class(lib_commune) != "character") {
    stop("Le libellé de commune doit être de type character", call. = FALSE)
  }

  test_longueur <- purrr::map_int(code_postal, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    message("Au moins un code postal n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  lib_commune <- caractr::str_remove_punct(lib_commune) %>%
    caractr::str_remove_accent() %>%
    toupper()

  conv_cp_ville_commune <- dplyr::left_join(dplyr::tibble(code_postal, lib_commune),
                                            geographie::cp_ville_commune,
                                            by = c("code_postal", "lib_commune")) %>%
    dplyr::pull(code_commune)

  return(conv_cp_ville_commune)
}

#' Obtenir le code postal a partir d'un code commune
#'
#' Obtenir le code postal à partir d'un code commune.
#'
#' @param code_commune Un vecteur de code commune de type caractère.
#'
#' @return Un vecteur de code postaux.\cr
#' Seuls les codes postaux non-doublonnés sont retournés.
#'
#' Jeu de données source : \code{geographie::ptt}.\cr
#' Il est créé à partir de la table "N_PTT" de la BCE.
#'
#' @examples
#' geographie::conv_commune_cp(c("18110", "38061"))
#'
#' @export
conv_commune_cp <- function(code_commune) {

  if (class(code_commune) != "character") {
    stop("Le code commune doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_commune)) %>% length() == 0) {
    return(code_commune)
  }

  test_longueur <- purrr::map_int(code_commune, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    message("Au moins un code commune n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  conv_commune_cp <- dplyr::select(geographie::ptt, code_commune, particularite_commune, code_postal) %>%
    dplyr::filter(particularite_commune %in% c(NA, "*")) %>%
    dplyr::select(code_commune, code_postal) %>%
    dplyr::group_by(code_commune) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::tibble(code_commune), ., by = "code_commune") %>%
    dplyr::pull(code_postal)

  return(conv_commune_cp)
}

#' Obtenir le code postal correspondant a une commune (a partir d'un code postal plus fin)
#'
#' Obtenir le code postal correspondant à une commune (à partir d'un code postal plus fin).
#'
#' @param code_postal Un vecteur de codes postaux de type caractère.
#'
#' @return Un vecteur de codes postaux correspondant à un code commune.
#'
#' Jeu de données source : \code{geographie::ptt}.\cr
#' Il est créé à partir de la table "N_PTT" de la BCE.
#'
#' @examples
#' geographie::conv_code_postal(c("75015", "75115"))
#'
#' @export
conv_code_postal <- function(code_postal) {

  if (class(code_postal) != "character") {
    stop("Le code postal doit être de type character", call. = FALSE)
  }

  test_longueur <- purrr::map_int(code_postal, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    message("Au moins un code postal n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  conv_code_postal <- dplyr::select(geographie::ptt, code_postal, code_commune) %>%
    dplyr::group_by(code_postal) %>%
    dplyr::filter(n() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::tibble(code_postal), ., by = "code_postal") %>%
    dplyr::mutate(code_postal = conv_commune_cp(code_commune)) %>%
    dplyr::pull(code_postal)

  return(conv_code_postal)
}

#' Obtenir le code pays INSEE a partir du code pays EU
#'
#' Obtenir le code pays INSEE à partir du code pays EU.
#'
#' @param code_pays_eu Un vecteur de code pays EU.
#'
#' Jeu de données source : \code{geographie::pays}.\cr
#' Il est créé à partir de la table "Pays" de la base Access "Tables_ref.accdb" (voir projet "Géographie").
#'
#' @return Un vecteur de code pays INSEE.
#'
#' @examples
#' geographie::conv_pays_eu_insee(c("FR", "DE"))
#'
#' @export
conv_pays_eu_insee <- function(code_pays_eu) {

  if (class(code_pays_eu) != "character") {
    stop("Le code pays doit être de type character", call. = FALSE)
  }

  if (which(!is.na(code_pays_eu)) %>% length() == 0) {
    return(code_pays_eu)
  }

  test_longueur <- purrr::map_int(code_pays_eu, nchar) %in% c(2, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    message("Au moins un code pays n'est pas de longueur 2: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  conv_pays_eu_insee <- dplyr::tibble(code_pays_eu) %>%
    dplyr::left_join(geographie::pays, by = c("code_pays_eu" = "code_pays_iso2")) %>%
    dplyr::pull(code_pays)

  return(conv_pays_eu_insee)
}

# conv_pays_insee_eu

#' Obtenir le code pays INSEE a partir de libelle de pays
#'
#' Obtenir le code pays INSEE à partir de libellés de pays.
#'
#' @param lib_pays Un vecteur de libellés de pays.
#'
#' Utilise less tables "Pays" et "Pays_libelle" de la base Access "Tables_ref.accdb".
#'
#' @return Un vecteur de code pays INSEE.
#'
#' @examples
#' geographie::conv_lib_code_pays(c("france", "Etats-Unis"))
#'
#' @export
conv_lib_code_pays <- function(lib_pays) {

  conv_lib_code_pays <- dplyr::tibble(lib_pays = lib_pays) %>%
    dplyr::mutate(lib_pays = tolower(lib_pays) %>%
                    caractr::str_remove_accent() %>%
                    caractr::str_remove_punct() %>%
                    stringr::str_replace_all("\\s+", " ")) %>%
    dplyr::left_join(tidyr::drop_na(geographie::pays, code_pays) %>%
                       dplyr::select(code_pays, lib_pays_fr, lib_pays_en) %>%
                       tidyr::gather("champ", "libelle_pays", -code_pays, na.rm = TRUE) %>%
                       dplyr::bind_rows(geographie::pays_libelle) %>%
                       dplyr::mutate(libelle_pays = tolower(libelle_pays) %>%
                                       caractr::str_remove_accent() %>%
                                       caractr::str_remove_punct() %>%
                                       stringr::str_replace_all("\\s+", " ")) %>%
                       dplyr::select(lib_pays = libelle_pays, code_pays) %>%
                       unique(),
                     by = "lib_pays") %>%
    dplyr::pull(code_pays)

  return(conv_lib_code_pays)
}
