#' Decouper une adresse complete en ligne
#'
#' Découper une adresse complète en ligne.
#'
#' @param adresse Un vecteur d'adresses.
#'
#' @return Une liste de vecteurs (taille de liste équivalente à la taille de \code{adresse}).\cr
#' Chaque élément de liste contient un vecteur dont la taille correspond au nombre de segment de l'adresse.
#'
#' Le découpage est réalisé selon trois critères :\cr
#'  - Le caractère de saut de ligne\cr
#'  - La présence d'un libellé de type voie renseigné dans la table \code{geographie::data_adresse_voie_prx}\cr
#'  - La présence d'une boite postale (BP) ou d'une course spéciale (CS)
#'
#' @examples
#' geographie::decouper_adresse_lignes(c("MENESR 1 Rue descartes", "1 Rue descartes BP 1"))
#'
#' @export
decouper_adresse_lignes <- function(adresse) {

  adresse_lignes <- dplyr::data_frame(adresse_init = adresse) %>%
    dplyr::mutate(cle = row_number(),
          adresse = adresse_init) %>%
    tidyr::separate_rows(adresse, sep = "\n")

  adresse_lignes <- dplyr::bind_cols(adresse_lignes, geographie::extraire_bp_cs(adresse_lignes$adresse)) %>%
    dplyr::select(cle, adresse = sans_bp_cs, bp_cs)

  regex_adresse <- dplyr::filter(geographie::data_adresse_voie_prx, !(lib_voie %in% c("b[aâ]t(iment)?", "mail", "moulin")))

  regex_adresse_1 <- dplyr::filter(regex_adresse, !lib_voie %in% c("campus", "cit[eé]", "dom(aine)?", "parc", "parvis", "plateau", "plt", "villa")) %>%
    .[["lib_voie"]] %>%
    paste(collapse = "|") %>%
    paste0("\\b((\\d+ )?(\\d+ ?(a|b(is)?|c|ter|d|e)?( +)?)?(", ., "))\\b .+") %>%
    stringr::regex(ignore_case = TRUE)

  regex_adresse_2 <- dplyr::filter(regex_adresse, lib_voie %in% c("campus", "cit[eé]", "dom(aine)?", "parc", "parvis", "plateau", "plt", "villa")) %>%
    .[["lib_voie"]] %>%
    paste(collapse = "|") %>%
    paste0("\\b(", ., ")\\b .+") %>%
    stringr::regex(ignore_case = TRUE)

  adresse_lignes <- dplyr::mutate(adresse_lignes,

                           position_debut = geographie::localiser_adresse(adresse, regex_adresse_1),
                           adresse_ligne_1 = ifelse(position_debut >= 2, substring(adresse, 1, position_debut - 1), NA_character_),
                           adresse_detectee_1 = ifelse(!is.na(position_debut), substring(adresse, position_debut), NA_character_),
                           adresse_ligne_1 = ifelse(is.na(position_debut), adresse, adresse_ligne_1),

                           position_debut = geographie::localiser_adresse(adresse_ligne_1, regex_adresse_2),
                           adresse_detectee_2 = ifelse(!is.na(position_debut), substring(adresse_ligne_1, position_debut), NA_character_),
                           adresse_ligne_2 = ifelse(position_debut >= 2, substring(adresse_ligne_1, 1, position_debut - 1), NA_character_),

                           adresse_ligne_1 = ifelse(!is.na(adresse_ligne_2), adresse_ligne_2, adresse_ligne_1),
                           adresse_ligne_1 = dplyr::if_else(position_debut == 1, NA_character_, adresse_ligne_1, adresse_ligne_1),

                           adresse_lignes = caractr::paste2(adresse_ligne_1, adresse_detectee_2, adresse_detectee_1, bp_cs, sep = "\n")
                           ) %>%
    dplyr::group_by(cle) %>%
    dplyr::summarise(adresse_lignes = caractr::paste2(adresse_lignes, collapse = "\n")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(adresse_lignes = stringr::str_split(adresse_lignes, pattern = "\n")) %>%
   .[["adresse_lignes"]]

  return(adresse_lignes)
}

#' Extraire d'une adresse la boite postale et/ou la course speciale (CS)
#'
#' Extraire d'une adresse la boite postale et/ou la course spéciale (CS).
#'
#' @param adresse Un vecteur d'adresses.
#'
#' @return Un data frame (nombre de lignes équivalent à la taille de \code{adresse}).\cr
#' Le champ \code{bp_cs} contient la BP/CS si celle-ci a été détectée.\cr
#' Le champ \code{sans_bp_cs} contient l'adresse originale, sans la BP/CS si celle-ci a été détectée.
#'
#' @export
#' @keywords internal
extraire_bp_cs <- function(adresse) {

  if (class(adresse) != "character") {
    stop("Le premier paramètre doit être de type character", call. = FALSE)
  }

  extraction_bp_cs <- lapply(adresse, stringr::str_locate, stringr::regex("((b\\.?p\\.?|c\\.?s\\.?|tsa)\\s*\\d+.*)", ignore_case = T)) %>%
    purrr::map_int(1) %>%
    dplyr::data_frame(adresse, position_debut = .) %>%
    dplyr::mutate(
      sans_bp_cs = ifelse(!is.na(position_debut),
                          substring(adresse, 1, position_debut - 1),
                          adresse),
      bp_cs = ifelse(!is.na(position_debut), substring(adresse, position_debut), NA)) %>%
    dplyr::select(-adresse, -position_debut)

  return(extraction_bp_cs)
}

#' Localiser une adresse dans une chaine de caracteres
#'
#' Localiser une adresse dans une chaine de caractères.
#'
#' @param adresse Un vecteur d'adresses.
#' @param regex_adresse Une expression régulière.
#'
#' @return Un vecteur numérique contenant la position de départ de l'adresse.
#'
#' @export
#' @keywords internal
localiser_adresse <- function(adresse, regex_adresse) {

  if (class(adresse) != "character") {
    stop("Le premier paramètre doit être de type character", call. = FALSE)
  }

  if (any(class(regex_adresse) == "regex") == FALSE) {
    stop("Le second paramètre doit être de type character (expression régulière)", call. = FALSE)
  }

  localisation <- caractr::sans_ponctuation(adresse) %>%
    stringr::str_locate_all(regex_adresse) %>%
    purrr::map( ~ .[, 1]) %>%
    purrr::map_dbl( ~ ifelse(length(.) == 0, NA, tail(., 1)))

  return(localisation)
}
