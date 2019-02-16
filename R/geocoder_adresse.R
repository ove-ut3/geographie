#' Obtenir le resultat des API (adresse.data.gouv.fr et google) a partir de l'adresse complete
#'
#' Obtenir le résultat des API (adresse.data.gouv.fr et google) à partir de l'adresse complète.
#'
#' @param adresse Un vecteur d'adresses.
#' @param service L'API à utiliser: \code{adresse.data.gouv.fr} ou \code{googlemaps}.
#' @param nettoyer_adresse \code{TRUE}, l'adresse est nettoyée (minuscule, sans accent et ponctuation); \code{FALSE}, aucune opération n'est réalisée.
#' @param timeout Temps de réponse maximal en secondes.
#' @param progress_bar Barre de progression (package pbapply)
#'
#' @return Un data_frame avec une ligne par adresse soumise.\cr
#'
#' Les résultats des géocodages sont accumulés et sauvegardés :\cr
#' - Dans \code{geographie::geocodage_data_gouv} pour le service \code{adresse.data.gouv.fr}\cr
#' - Dans \code{geographie::geocodage_google} pour le service \code{googlemaps}\cr
#'
#' Pour le service \code{adresse.data.gouv.fr}, le data frame contient les champs suivants :\cr
#' - \code{adresse} : adresse initiale\cr
#' - \code{adresse_nettoyee} : adresse nettoyée\cr
#' - \code{date_ajout} : date de téléchargement\cr
#' (Les champs suivants sont les éléments retournés par l'API)\cr
#' - \code{match}\cr
#' - \code{score}\cr
#' - \code{type}\cr
#' - \code{latitude}\cr
#' - \code{longitude}\cr
#' - \code{code_postal}\cr
#' - \code{code_commune}\cr
#' - \code{type_localisation}\cr
#' - \code{numero_voie}\cr
#' - \code{lib_voie}\cr
#' - \code{localite}\cr
#'
#' Pour le service \code{googlemaps}, le data frame contient les champs suivants :\cr
#' - \code{adresse} : adresse initiale\cr
#' - \code{adresse_nettoyee} : adresse nettoyée\cr
#' - \code{date_ajout} : date de téléchargement\cr
#' (Les champs suivants sont les éléments retournés par l'API)\cr
#' - \code{latitude}\cr
#' - \code{longitude}\cr
#' - \code{type_localisation}\cr
#' - \code{statut}\cr
#'
#' @examples
#' # Exemple avec le service adresse.gouv pour le MENESR et Rennes 2
#' geographie::geocoder_adresse(c("1 Rue Descartes 75005 Paris", "Place Recteur Henri le Moal 35000 Rennes"),
#'   service = "adresse.data.gouv.fr")
#'
#' # Exemple avec le service google pour le MENESR et Rennes 2
#' geographie::geocoder_adresse(c("1 Rue Descartes 75005 Paris", "Place Recteur Henri le Moal 35000 Rennes"),
#'   service = "googlemaps")

#' @export
geocoder_adresse <- function(adresse, service, nettoyer_adresse = TRUE, timeout = 10, progress_bar = FALSE) {

  if (!service %in% c("adresse.data.gouv.fr", "googlemaps")) {
    stop("Le service doit être \"adresse.data.gouv.fr\" ou \"googlemaps\"", call. = FALSE)
  }

  geocoder_init <- dplyr::tibble(adresse)

  message("Géocodage \"", service, "\" : ", nrow(geocoder_init), " adresses soumises")

  if (nettoyer_adresse == TRUE) {

    geocoder_init <- dplyr::mutate(geocoder_init,
                                   adresse_nettoyee = tolower(adresse) %>%
                                     caractr::str_remove_accent() %>%
                                     stringr::str_replace_all("[[:punct:][:cntrl:]]", " ") %>%
                                     trimws() %>%
                                     stringr::str_replace_all("\\s+", " ")
    )

  } else geocoder_init = dplyr::mutate(geocoder_init, adresse_nettoyee = adresse)

  if (service == "adresse.data.gouv.fr") {
    base_geocodage <- impexp::r_import("data/geocodage_data_gouv.RData")
  } else if (service == "googlemaps") base_geocodage <- impexp::r_import("data/geocodage_google.RData")

  geocoder_ajout <- dplyr::anti_join(geocoder_init, base_geocodage, by = c("adresse_nettoyee" = "adresse"))

  if (nrow(geocoder_ajout) == 0) {

    message("Pas de nouvelle adresse par rapport à la base de géocodage")
    message("")
    geocoder <- dplyr::left_join(geocoder_init, base_geocodage, by = c("adresse_nettoyee" = "adresse"))
    return(geocoder)

  } else geocoder <- geocoder_ajout

  message("Géocodage de ", length(geocoder$adresse_nettoyee %>% unique()), " adresses distinctes.")

  if (service == "adresse.data.gouv.fr") {
    geocoder <- geocoder_adresse_data_gouv(adresse = geocoder$adresse_nettoyee %>% unique(), timeout = timeout, progress_bar = progress_bar)

  } else if (service == "googlemaps") {
    geocoder <- geocoder_adresse_google(adresse = geocoder$adresse_nettoyee %>% unique(), timeout = timeout, progress_bar = progress_bar)

  }

  if (nrow(geocoder) == length(adresse)) {

    geocoder <- unique(geocoder) %>%
      dplyr::left_join(geocoder_init, ., by = c("adresse_nettoyee" = "adresse"))

    return(geocoder)
  } else {

    geocoder <- dplyr::bind_rows(geocoder, base_geocodage) %>%
      unique() %>%
      dplyr::left_join(geocoder_init, ., by = c("adresse_nettoyee" = "adresse"))

    return(geocoder)
  }

}
