#' Obtenir le resultat des API (adresse.data.gouv.fr et google) a partir de l'adresse complete
#'
#' Obtenir le résultat des API (adresse.data.gouv.fr et google) à partir de l'adresse complète.
#'
#' @param adresse Un vecteur d'adresses.
#' @param service L'API à utiliser: \code{adresse.data.gouv.fr} ou \code{googlemaps}.
#' @param nettoyer_adresse \code{TRUE}, l'adresse est nettoyée (minuscule, sans accent et ponctuation); \code{FALSE}, aucune opération n'est réalisée.
#' @param timeout Temps de réponse maximal en secondes.
#'
#' @return Un data_frame avec une ligne par adresse soumise.\cr
#'
#' Les résultats des géocodages sont accumulés et sauvegardés :\cr
#' - Dans \code{geographie::data_geocodage_data_gouv} pour le service \code{adresse.data.gouv.fr}\cr
#' - Dans \code{geographie::data_geocodage_google} pour le service \code{googlemaps}\cr
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
geocoder_adresse <- function(adresse, service, nettoyer_adresse = TRUE, timeout = 10, cle_google = NULL) {

  if (!service %in% c("adresse.data.gouv.fr", "googlemaps")) {
    stop("Le service doit être \"adresse.data.gouv.fr\" ou \"googlemaps\"", call. = FALSE)
  }

  #adresse <- geocodage_google$adresse
  geocoder_init <- dplyr::data_frame(adresse)

  message("Géocodage \"", service, "\" : ", nrow(geocoder_init), " adresses soumises")

  if (nettoyer_adresse == TRUE) {

    geocoder_init <- dplyr::mutate(geocoder_init,
                                   adresse_nettoyee = tolower(adresse) %>%
                                     caractr::sans_accent() %>%
                                     stringr::str_replace_all("[[:punct:][:cntrl:]]", " ") %>%
                                     trimws() %>%
                                     stringr::str_replace_all("\\s+", " ")
    )

  } else geocoder_init = dplyr::mutate(geocoder_init, adresse_nettoyee = adresse)

  if (service == "adresse.data.gouv.fr") {
    base_geocodage <- charger_rdata(paste0(racine_packages, "geographie/data/data_geocodage_data_gouv.RData"), "data_geocodage_data_gouv")
  } else if (service == "googlemaps") base_geocodage <- charger_rdata(paste0(racine_packages, "geographie/data/data_geocodage_google.RData"), "data_geocodage_google")

  #base_geocodage <- dplyr::filter(base_geocodage, divr::mois_ecoules(date_ajout, Sys.Date()) <= 12)
  geocoder_ajout <- dplyr::anti_join(geocoder_init, base_geocodage, by = c("adresse_nettoyee" = "adresse"))

  if (nrow(geocoder_ajout) == 0) {

    message("Pas de nouvelle adresse par rapport à la base de géocodage")
    message("")
    geocoder <- dplyr::left_join(geocoder_init, base_geocodage, by = c("adresse_nettoyee" = "adresse"))
    return(geocoder)

  } else geocoder <- geocoder_ajout

  message("Géocodage de ", length(geocoder$adresse_nettoyee %>% unique()), " adresses distinctes.")

  if (service == "adresse.data.gouv.fr") {
    geocoder <- geocoder_adresse_data_gouv(adresse = geocoder$adresse_nettoyee %>% unique(), timeout = timeout)

  } else if (service == "googlemaps") {
    geocoder <- geocoder_adresse_google(adresse = geocoder$adresse_nettoyee %>% unique(), timeout = timeout, cle_google = cle_google)

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

#' Geocoder des adresses a partir de l'API adresse.data.gouv.fr
#'
#' Géocoder des adresses à partir de l'API adresse.data.gouv.fr.
#'
#' @param adresse Un vecteur d'adresses.
#' @param nettoyer_adresse \code{TRUE}, l'adresse est nettoyée (minuscule, sans accent et ponctuation); \code{FALSE}, aucune opération n'est réalisée.
#' @param timeout Temps de réponse maximal en secondes.
#'
#' @return A matrix of the infile
#'
#' @export
#' @keywords internal
geocoder_adresse_data_gouv <- function(adresse, timeout = 10) {

  geocoder <- dplyr::data_frame(adresse) %>%
    dplyr::mutate(appel_api = stringr::str_replace_all(adresse, " ", "+") %>%
                    paste0("http://api-adresse.data.gouv.fr/search/?limit=1&q=", .)
                  )

  geocodage <- pbapply::pblapply(geocoder$appel_api %>% unique(),
                          purrr::safely(webr::telecharger_url),
                          timeout = timeout,
                          format_api = "json")

  geocoder <- dplyr::data_frame(appel_api = geocoder$appel_api %>% unique(),
                                resultat = purrr::map(geocodage, "result"),
                                erreur = purrr::map(geocodage, "error")) %>%
    dplyr::left_join(geocoder, ., by = "appel_api")


  geocoder <- dplyr::mutate(geocoder, date_ajout = Sys.Date())

  geocoder <- dplyr::mutate(geocoder, match = purrr::map_lgl(resultat, ~ .$features %>% length() != 0))

  geocoder <- dplyr::filter(geocoder, match) %>%
    dplyr::select(appel_api, resultat) %>%
    unique() %>%
    dplyr::mutate(score = purrr::map_dbl(resultat, ~ .$features$properties$score),
                  type = purrr::map_chr(resultat, ~ .$features$geometry$type),
                  latitude = purrr::map_dbl(resultat, ~ .$features$geometry$coordinates[[1]][2]),
                  longitude = purrr::map_dbl(resultat, ~ .$features$geometry$coordinates[[1]][1]),
                  code_postal = purrr::map_chr(resultat, ~ .$features$properties$postcode),
                  code_commune = purrr::map_chr(resultat, ~ .$features$properties$citycode),
                  type_localisation = purrr::map_chr(resultat, ~ .$features$properties$type)) %>%
    dplyr::select(-resultat) %>%
    dplyr::left_join(geocoder, ., by = "appel_api")

  geocoder <- dplyr::filter(geocoder, type_localisation == "housenumber") %>%
    dplyr::select(appel_api, resultat) %>%
    unique() %>%
    dplyr::mutate(numero_voie = purrr::map_chr(resultat, ~ .$features$properties$housenumber),
                  lib_voie = purrr::map_chr(resultat, ~ ifelse(!is.null(.$features$properties$street), .$features$properties$street, NA_character_)),
                  localite = purrr::map_chr(resultat, ~ ifelse(!is.null(.$features$properties$locality), .$features$properties$locality, NA_character_))) %>%
    dplyr::select(-resultat) %>%
    dplyr::left_join(geocoder, ., by = "appel_api")

  geocoder <- dplyr::filter(geocoder, type_localisation == "street") %>%
    dplyr::select(appel_api, resultat) %>%
    unique() %>%
    dplyr::mutate(lib_voie_rue = purrr::map_chr(resultat, ~ .$features$properties$name)) %>%
    dplyr::select(-resultat) %>%
    dplyr::left_join(geocoder, ., by = "appel_api") %>%
    dplyr::mutate(lib_voie = ifelse(!is.na(lib_voie_rue), lib_voie_rue, lib_voie)) %>%
    dplyr::select(-lib_voie_rue)

  geocoder <- dplyr::filter(geocoder, type_localisation == "locality") %>%
    dplyr::select(appel_api, resultat) %>%
    unique() %>%
    dplyr::mutate(localite_locality = purrr::map_chr(resultat, ~ .$features$properties$name)) %>%
    dplyr::select(-resultat) %>%
    dplyr::left_join(geocoder, ., by = "appel_api") %>%
    dplyr::mutate(localite = ifelse(!is.na(localite_locality), localite_locality, localite)) %>%
    dplyr::select(-localite_locality)

  geocoder <- dplyr::select(geocoder, -appel_api, -resultat)

  data_geocodage_data_gouv <- dplyr::filter(geocoder, purrr::map_lgl(erreur, is.null)) %>%
    dplyr::select(-erreur) %>%
    dplyr::bind_rows(charger_rdata(paste0(racine_packages, "geographie/data/data_geocodage_data_gouv.RData"), "data_geocodage_data_gouv")) %>%
    dplyr::arrange(adresse)

  message("Sauvegarde package \"geographie\": ", nrow(data_geocodage_data_gouv), " adresses au total")
  message("")
  save("data_geocodage_data_gouv", file = paste0(racine_packages, "geographie/data/data_geocodage_data_gouv.RData"))

  return(geocoder)
}

#' Geocoder des adresses a partir de l'API google
#'
#' Géocoder des adresses à partir de l'API google.
#'
#' @param adresse Un vecteur d'adresses.
#' @param timeout Temps de réponse maximal en secondes.
#'
#' @return A matrix of the infile
#'
#' @export
#' @keywords internal
geocoder_adresse_google <- function(adresse, timeout = 10, cle_google) {

  geocoder <- dplyr::data_frame(adresse) %>%
    dplyr::mutate(appel_api = stringr::str_replace_all(adresse, " ", "+") %>%
                    paste0("https://maps.googleapis.com/maps/api/geocode/json?key=", cle_google, "&address=", .)
    )

  # Test dépassement quota
  test_quota <- webr::telecharger_url(geocoder$appel_api[1], format_api = "json") %>%
    .[["status"]]

  if (test_quota == "OVER_QUERY_LIMIT") {
    message("Aucun géocodage : quota de 2500 requêtes par jour dépassé")
    geocoder <- left_join(geocoder_init,
                          charger_rdata(paste0(racine_packages, "geographie/data/data_geocodage_google.RData"), "data_geocodage_google"),
                          by = c("adresse_nettoyee" = "adresse"))
    return(geocoder)
  }

  geocodage <- pbapply::pblapply(geocoder$appel_api %>% unique(),
                                 purrr::safely(webr::telecharger_url),
                                 timeout = timeout,
                                 format_api = "json")

  geocoder <- dplyr::data_frame(appel_api = geocoder$appel_api %>% unique(),
                                resultat = purrr::map(geocodage, "result"),
                                erreur = purrr::map_chr(geocodage, ~ ifelse(is.null(.$error), "", .$error))) %>%
    dplyr::left_join(geocoder, ., by = "appel_api")


  geocoder <- dplyr::mutate(geocoder, date_ajout = Sys.Date())

  geocoder <- dplyr::mutate(geocoder, match = purrr::map_lgl(resultat, ~ .$results %>% length() != 0))

  geocoder <- dplyr::filter(geocoder, match) %>%
    dplyr::select(appel_api, resultat) %>%
    unique() %>%
    dplyr::mutate(latitude = purrr::map_dbl(resultat, ~ .$results$geometry$location$lat %>%
                                              tail(1)),
                  longitude = purrr::map_dbl(resultat, ~ .$results$geometry$location$lng %>%
                                               tail(1)),
                  type_localisation = purrr::map_chr(resultat, ~ .$results$geometry$location_type %>%
                                                       tail(1)),
                  statut = purrr::map_chr(resultat, ~ .$status)) %>%
    dplyr::select(-resultat) %>%
    dplyr::left_join(geocoder, ., by = "appel_api") %>%
    select(adresse, latitude, longitude, type_localisation, statut, erreur, date_ajout)

  data_geocodage_google <- dplyr::filter(geocoder, !nchar(erreur) != 0) %>%
    dplyr::select(-erreur, -statut) %>%
    dplyr::bind_rows(charger_rdata(paste0(racine_packages, "geographie/data/data_geocodage_google.RData"), "data_geocodage_google")) %>%
    dplyr::arrange(adresse)

  message("Sauvegarde package \"geographie\": ", nrow(data_geocodage_google), " adresses au total")
  message("")
  save("data_geocodage_google", file = paste0(racine_packages, "geographie/data/data_geocodage_google.RData"))

  if (any(geocodage$status == "OVER_QUERY_LIMIT") == TRUE) {
    message("Géocodage partiel : quota de 2500 requêtes par jour dépassé")
  }

  return(geocoder)
}
