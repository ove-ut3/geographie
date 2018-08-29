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
#'  - La présence d'un libellé de type voie renseigné dans la table \code{geographie::adresse_voie_prx}\cr
#'  - La présence d'une boite postale (BP) ou d'une course spéciale (CS)
#'
#' @examples
#' geographie::decouper_adresse_lignes(c("MENESR 1 Rue descartes", "1 Rue descartes BP 1"))
#'
#' @export
decouper_adresse_lignes <- function(adresse) {

  adresse_lignes <- dplyr::tibble(adresse_init = adresse) %>%
    dplyr::mutate(cle = dplyr::row_number(),
          adresse = adresse_init) %>%
    tidyr::separate_rows(adresse, sep = "\n")

  adresse_lignes <- dplyr::bind_cols(adresse_lignes, extraire_bp_cs(adresse_lignes$adresse)) %>%
    dplyr::select(cle, adresse = sans_bp_cs, bp_cs)

  regex_adresse <- dplyr::filter(geographie::adresse_voie_prx, !(lib_voie %in% c("b[aâ]t(iment)?", "mail", "moulin")))

  regex_adresse_1 <- dplyr::filter(regex_adresse, !lib_voie %in% c("campus", "cit[eé]", "dom(aine)?", "parc", "parvis", "plateau", "plt", "villa")) %>%
    dplyr::pull(lib_voie) %>%
    paste(collapse = "|") %>%
    paste0("\\b((\\d+ )?(\\d+ ?(a|b(is)?|c|ter|d|e)?( +)?)?(", ., "))\\b .+") %>%
    stringr::regex(ignore_case = TRUE)

  regex_adresse_2 <- dplyr::filter(regex_adresse, lib_voie %in% c("campus", "cit[eé]", "dom(aine)?", "parc", "parvis", "plateau", "plt", "villa")) %>%
    dplyr::pull(lib_voie) %>%
    paste(collapse = "|") %>%
    paste0("\\b(", ., ")\\b .+") %>%
    stringr::regex(ignore_case = TRUE)

  adresse_lignes <- dplyr::mutate(adresse_lignes,

                           position_debut = localiser_adresse(adresse, regex_adresse_1),
                           adresse_ligne_1 = ifelse(position_debut >= 2, substring(adresse, 1, position_debut - 1), NA_character_),
                           adresse_detectee_1 = ifelse(!is.na(position_debut), substring(adresse, position_debut), NA_character_),
                           adresse_ligne_1 = ifelse(is.na(position_debut), adresse, adresse_ligne_1),

                           position_debut = localiser_adresse(adresse_ligne_1, regex_adresse_2),
                           adresse_detectee_2 = ifelse(!is.na(position_debut), substring(adresse_ligne_1, position_debut), NA_character_),
                           adresse_ligne_2 = ifelse(position_debut >= 2, substring(adresse_ligne_1, 1, position_debut - 1), NA_character_),

                           adresse_ligne_1 = ifelse(!is.na(adresse_ligne_2), adresse_ligne_2, adresse_ligne_1),
                           adresse_ligne_1 = dplyr::if_else(position_debut == 1, NA_character_, adresse_ligne_1, adresse_ligne_1),

                           adresse_lignes = caractr::str_paste(adresse_ligne_1, adresse_detectee_2, adresse_detectee_1, bp_cs, sep = "\n")
                           ) %>%
    dplyr::group_by(cle) %>%
    dplyr::summarise(adresse_lignes = caractr::str_paste(adresse_lignes, collapse = "\n")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(adresse_lignes = stringr::str_split(adresse_lignes, pattern = "\n")) %>%
    dplyr::pull(adresse_lignes)

  return(adresse_lignes)
}
