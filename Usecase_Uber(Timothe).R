
# Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)
library(caret)
library(FactoMineR)
library(statip)

# Chargement des bibliothèques pour visualisation (si nécessaire)
library(ggplot2)       # Pour des graphiques et visualisations personnalisées
library(lattice)       # Pour des graphiques de type trellis

clean_data <- function(data) {
  # Traiter les valeurs manquantes
  data <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    mutate(across(where(is.factor), ~ ifelse(is.na(.), as.character(statip::mfv(.)), .)))
  
  # Encodage limité aux colonnes essentielles
  important_columns <- names(data)  # Remplacez par vos colonnes essentielles
  data <- data[important_columns]
  
  # Encodage one-hot
  data <- caret::dummyVars(" ~ .", data = data) %>%
    predict(newdata = data) %>%
    as.data.frame()
  
  # Réduction de dimension par ACP
  acp <- FactoMineR::PCA(data, scale.unit = TRUE, ncp = 5, graph = FALSE)
  
  
  # Réduction de dimension sur un échantillon
  set.seed(123)
  sampled_data <- data[sample(1:nrow(data), min(10000, nrow(data))), ]
  acp <- FactoMineR::PCA(sampled_data, scale.unit = TRUE, ncp = 5, graph = FALSE)
  data_reduced <- as.data.frame(acp$ind$coord)
  
  # Conserver les données encodées pour un accès ultérieur
  return(list(encoded_data = data, reduced_data = data_reduced))
  return(data_reduced)
}

# Fonction pour charger toutes les données
get_data <- function() {
  months <- c("apr14", "aug14", "jul14", "jun14", "may14", "sep14")
  file_paths <- file.path("UberDatasets", paste0("uber-raw-data-", months, ".csv"))
  #print(file_paths)
  
  data_list <- lapply(file_paths, function(path) {
    if (file.exists(path)) {
      read_data <- read.csv(path)  # Lire les données ici
      return(clean_data(read_data))  # Passer le DataFrame lu à clean_data
    } else {
      message("File not found: ", path)
      return(NULL)
    }
  })
  
  names(data_list) <- months
  data_list <- data_list[!sapply(data_list, is.null)]  # Retirer les fichiers manquants
  
  return(data_list)
}

#######FONCTION POUR RECUPERER UN MOIS OU UNE DATA SPECIFIC
get_spec_data<-function(month, type_data = "reduced_data"){
  data_all<-get_data()
  month_data<-data_all[[month]]
  
  if (!month %in% names(data_all)) {
    stop("Error: Invalid month. Valid options are: ", paste(names(data_all), collapse = ", "))
  }
  
  if (is.null(type_data)){
    return(month_data)
  }
  
  if (!type_data %in% names(month_data)) {
    stop("Error: Invalid type_data. Valid options are: ", paste(names(month_data), collapse = ", "))
  }
  
  return(month_data[[type_data]])
  
}

######FONCTION POUR AFFICHER UN SCHEMA


#####FONCTION POUR

raw_data <- read.csv("UberDatasets/uber-raw-data-jun14.csv")
cleaned_data <- clean_data(raw_data)
#data_list <- get_data()  # Obtenez une liste de données pour différents mois
data_list <- get_data()  # Chargez toutes les données dans une variable globale
jun_data <- data_list[["jun14"]]  # Récupérez les données de juin 2014
# Nettoyer et transformer les données pour un mois spécifique
jun_data_encoded <- data_list[["jun14"]]$encoded_data  # Données encodées
cleaned_jun_data <- clean_data(jun_data_encoded)  # Nettoyer et réduire les données
# Afficher les résultats
print(head(cleaned_jun_data))
#str(data_all$jun14)  # Vérifiez les données pour "jun14"
jun_data <- get_spec_data("jun14")
# Charger les données
data_test <- get_data()

# Vérifiez si data_test existe
if (exists("data_test") && !is.null(data_test)) {
  print("Les données ont été chargées avec succès.")
  
  # Vérifiez les mois disponibles
  print(names(data_test))
  
  # Accédez à un mois spécifique
  month <- "jun14"
  if (month %in% names(data_test)) {
    jun_data <- data_test[[month]]
    print(head(jun_data$reduced_data))  # Affiche les données réduites pour juin
  } else {
    print("Le mois 'jun14' n'est pas dans les données.")
  }
} else {
  print("Erreur : data_test n'a pas été chargé correctement.")
}


message("Data successfully loaded for month: ", month)
print(head(jun_data))  # Afficher les premières lignes des données
print(names(data_test))  # Voir les mois disponibles
#data_test$apr14[1:10, ]

