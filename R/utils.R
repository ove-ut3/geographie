extraire_bp_cs <- function(adresse) {

  if (class(adresse) != "character") {
    stop("Le premier paramètre doit être de type character", call. = FALSE)
  }

  extraction_bp_cs <- lapply(adresse, stringr::str_locate, stringr::regex("((b\\.?p\\.?|c\\.?s\\.?|tsa)\\s*\\d+.*)", ignore_case = TRUE)) %>%
    purrr::map_int(1) %>%
    dplyr::tibble(adresse, position_debut = .) %>%
    dplyr::mutate(
      sans_bp_cs = ifelse(!is.na(position_debut),
                          substring(adresse, 1, position_debut - 1),
                          adresse),
      bp_cs = ifelse(!is.na(position_debut), substring(adresse, position_debut), NA)) %>%
    dplyr::select(-adresse, -position_debut)

  return(extraction_bp_cs)
}

localiser_adresse <- function(adresse, regex_adresse) {

  if (class(adresse) != "character") {
    stop("Le premier paramètre doit être de type character", call. = FALSE)
  }

  if (any(class(regex_adresse) == "regex") == FALSE) {
    stop("Le second paramètre doit être de type character (expression régulière)", call. = FALSE)
  }

  localisation <- adresse %>%
    stringr::str_remove_all("[[:punct:]]+") %>%
    stringr::str_locate_all(regex_adresse) %>%
    purrr::map( ~ .[, 1]) %>%
    purrr::map_dbl( ~ ifelse(length(.) == 0, NA, tail(., 1)))

  return(localisation)
}

geocoder_adresse_data_gouv <- function(adresse, timeout = 10, progress_bar = FALSE) {

  geocoder <- dplyr::tibble(adresse) %>%
    dplyr::mutate(appel_api = stringr::str_replace_all(adresse, " ", "+") %>%
                    paste0("http://api-adresse.data.gouv.fr/search/?limit=1&q=", .)
    )

  if (progress_bar == TRUE) {
    fn_apply <- pbapply::pblapply
  } else {
    fn_apply <- lapply
  }

  geocodage <- fn_apply(geocoder$appel_api %>% unique(),
                        purrr::safely(webr::download_url),
                        timeout = timeout,
                        api_format = "json")

  geocoder <- dplyr::tibble(appel_api = geocoder$appel_api %>% unique(),
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

  geocodage_data_gouv <- dplyr::filter(geocoder, purrr::map_lgl(erreur, is.null)) %>%
    dplyr::select(-erreur) %>%
    dplyr::bind_rows(impexp::r_import("data/geocodage_data_gouv.RData")) %>%
    dplyr::arrange(adresse)

  message("Sauvegarde package \"geographie\": ", nrow(data_geocodage_data_gouv), " adresses au total")
  message("")
  save("geocodage_data_gouv", file = "data/geocodage_data_gouv.RData")

  return(geocoder)
}

geocoder_adresse_google <- function(adresse, timeout = 10, progress_bar = FALSE) {

  geocoder <- dplyr::tibble(adresse) %>%
    dplyr::mutate(appel_api = stringr::str_replace_all(adresse, " ", "+") %>%
                    paste0("https://maps.googleapis.com/maps/api/geocode/json?key=", cle_google, "&address=", .)
    )

  # Test dépassement quota
  test_quota <- webr::download_url(geocoder$appel_api[1], api_format = "json") %>%
    dplyr::pull(status)

  if (test_quota == "OVER_QUERY_LIMIT") {
    warning("Aucun géocodage : quota de 2500 requêtes par jour dépassé")
    geocoder <- left_join(geocoder_init,
                          impexp::r_import("data/geocodage_google.RData"),
                          by = c("adresse_nettoyee" = "adresse"))
    return(geocoder)
  }

  if (progress_bar == TRUE) {
    fn_apply <- pbapply::pblapply
  } else {
    fn_apply <- lapply
  }

  geocodage <- fn_apply(geocoder$appel_api %>% unique(),
                        purrr::safely(webr::download_url),
                        timeout = timeout,
                        api_format = "json")

  geocoder <- dplyr::tibble(appel_api = geocoder$appel_api %>% unique(),
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
    dplyr::select(adresse, latitude, longitude, type_localisation, statut, erreur, date_ajout)

  geocodage_google <- dplyr::filter(geocoder, !nchar(erreur) != 0) %>%
    dplyr::select(-erreur, -statut) %>%
    dplyr::bind_rows(impexp::r_import("data/geocodage_google.RData")) %>%
    dplyr::arrange(adresse)

  message("Sauvegarde package \"geographie\": ", nrow(geocodage_google), " adresses au total")
  message("")
  save("geocodage_google", file = "data/geocodage_google.RData")

  if (any(geocodage$status == "OVER_QUERY_LIMIT") == TRUE) {
    message("Géocodage partiel : quota de 2500 requêtes par jour dépassé")
  }

  return(geocoder)
}
