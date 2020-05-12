#' Renvoie le code d'unite urbaine a partir du code commune
#'
#' Renvoie le code d'unité urbaine à partir du code commune.
#'
#' @param code_commune Un vecteur de code commune.
#'
#' @return Un vecteur de code d'unité urbaine.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' # Un exemple avec deux communes, l'une dans une unité urbaine, l'autre non
#' geographie::hier_commune_uu(c("01001", "33003"))
#'
#' @export
hier_commune_uu <- function(code_commune) {

  if (class(code_commune) != "character") {
    stop("Le code commune doit \u00eatre de type character", call. = FALSE)
  }

  if (which(!is.na(code_commune)) %>% length() == 0) {
    return(code_commune)
  }

  test_longueur <- purrr::map_int(code_commune, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code commune n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  hier_commune_uu <- dplyr::tibble(code_commune) %>%
    dplyr::left_join(
      geographie::ods_geo %>%
        dplyr::select(.data$code_commune, .data$code_uu),
      by = "code_commune"
    ) %>%
    dplyr::pull(.data$code_uu)

  return(hier_commune_uu)
}

#' Obtenir le code departement a partir d'un code commune
#'
#' Obtenir le code département à partir d'un code commune.
#'
#' @param code_commune Un vecteur de code commune.
#'
#' @return Un vecteur de code de département.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::hier_commune_departement(c("01001", "33003"))
#'
#' @export
hier_commune_departement <- function(code_commune) {

  if (class(code_commune) != "character") {
    stop("Le code commune doit \u00eatre de type character", call. = FALSE)
  }

  if (which(!is.na(code_commune)) %>% length() == 0) {
    return(code_commune)
  }

  test_longueur <- purrr::map_int(code_commune, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code commune n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  conv_commune_departement <- dplyr::tibble(code_commune) %>%
    dplyr::left_join(
      geographie::ods_geo %>%
        dplyr::select(.data$code_commune, .data$code_departement),
      by = "code_commune"
    ) %>%
    dplyr::pull(.data$code_departement)

  return(conv_commune_departement)
}

#' Obtenir le code region a partir d'un code commune
#'
#' Obtenir le code région à partir d'un code commune.
#'
#' @param code_commune Un vecteur de code commune.
#'
#' @return Un vecteur de code de région.
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::hier_commune_region(c("01001", "33003"))
#'
#' @export
hier_commune_region <- function(code_commune) {

  if (class(code_commune) != "character") {
    stop("Le code commune doit \u00eatre de type character", call. = FALSE)
  }

  if (which(!is.na(code_commune)) %>% length() == 0) {
    return(code_commune)
  }

  test_longueur <- purrr::map_int(code_commune, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code commune n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  conv_commune_region <- dplyr::tibble(code_commune) %>%
    dplyr::left_join(
      geographie::ods_geo %>%
        dplyr::select(.data$code_commune, .data$code_region),
      by = "code_commune"
    ) %>%
    dplyr::pull(.data$code_region)

  return(conv_commune_region)
}

#' Obtenir le code region (2015 et avant) a partir d'un code commune
#'
#' Obtenir le code région (2015 et avant) à partir d'un code commune.
#'
#' @param code_commune Un vecteur de code commune.
#'
#' @return Un vecteur de code de région (2015 et avant).
#'
#' Jeu de données source : \code{geographie::ods_geo}.\cr
#'
#' @examples
#' geographie::hier_commune_region_2015(c("01001", "33003"))
#'
#' @export
hier_commune_region_2015 <- function(code_commune) {

  if (class(code_commune) != "character") {
    stop("Le code commune doit \u00eatre de type character", call. = FALSE)
  }

  if (which(!is.na(code_commune)) %>% length() == 0) {
    return(code_commune)
  }

  test_longueur <- purrr::map_int(code_commune, nchar) %in% c(5, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code commune n'est pas de longueur 5: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  conv_commune_region_2015 <- dplyr::tibble(code_commune) %>%
    dplyr::left_join(
      geographie::ods_geo %>%
        dplyr::select(.data$code_commune, .data$code_region_2015),
      by = "code_commune"
    ) %>%
    dplyr::pull(.data$code_region_2015)

  return(conv_commune_region_2015)
}

#' Obtenir le code region a partir d'un code departement
#'
#' Obtenir le code région à partir d'un code département.
#'
#' @param code_departement Un vecteur de code département.
#'
#' @return Un vecteur de code de région.
#'
#' Jeu de données source : \code{geographie::departement}.\cr
#'
#' @examples
#' geographie::hier_departement_region(c("003", "056"))
#'
#' @export
hier_departement_region <- function(code_departement) {

  if (class(code_departement) != "character") {
    stop("Le code d\u00e9partement doit \u00eatre de type character", call. = FALSE)
  }

  if (which(!is.na(code_departement)) %>% length() == 0) {
    return(code_departement)
  }

  test_longueur <- purrr::map_int(code_departement, nchar) %in% c(3, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code d\u00e9partement n'est pas de longueur 3: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  hier_departement_region <- dplyr::tibble(code_departement) %>%
    dplyr::left_join(
      geographie::departement %>%
        dplyr::select(.data$code_departement, .data$code_region),
      by = "code_departement"
    ) %>%
    dplyr::pull(.data$code_region)

  return(hier_departement_region)
}

#' Obtenir le code region (2015 et avant) a partir d'un code departement
#'
#' Obtenir le code région (2015 et avant) à partir d'un code département.
#'
#' @param code_departement Un vecteur de code département.
#'
#' @return Un vecteur de code de région (2015 et avant).
#'
#' Jeu de données source : \code{geographie::departement}.\cr
#'
#' @examples
#' geographie::hier_departement_region_2015(c("003", "056"))
#'
#' @export
hier_departement_region_2015 <- function(code_departement) {

  if (class(code_departement) != "character") {
    stop("Le code d\u00e9partement doit \u00eatre de type character", call. = FALSE)
  }

  if (which(!is.na(code_departement)) %>% length() == 0) {
    return(code_departement)
  }

  test_longueur <- purrr::map_int(code_departement, nchar) %in% c(3, NA_integer_)
  if (all(test_longueur, na.rm = TRUE) == FALSE) {
    warning("Au moins un code d\u00e9partement n'est pas de longueur 3: positions [", paste(which(!test_longueur), collapse = ", "), "]")
  }

  hier_departement_region_2015 <- dplyr::tibble(code_departement) %>%
    dplyr::left_join(
      geographie::departement %>%
        dplyr::select(.data$code_departement, .data$code_region_2015),
      by = "code_departement"
    ) %>%
    dplyr::pull(.data$code_region_2015)

  return(hier_departement_region_2015)
}
