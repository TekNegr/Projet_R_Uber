install.packages(c("dplyr", "ggplot2", "lubridate"))
library(dplyr)
library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)


get_data <- function(folder_name) {
  months <- c("apr14", "aug14", "jul14", "jun14", "may14", "sep14")
  file_paths <- file.path(folder_name, paste0("uber-raw-data-", months, ".csv"))
  
  # Vérifier si les fichiers existent
  if (!all(file.exists(file_paths))) {
    stop("Error: Some files are missing in the specified folder. Please check the folder path.")
  }
  
  # Lire tous les fichiers CSV
  data_list <- lapply(file_paths, read.csv)
  
  # Ajouter des noms aux données
  names(data_list) <- months
  
  return(data_list)
}



#######FONCTION POUR RECUPERER UN MOIS OU UNE DATA SPECIFIC
get_spec_data <- function(month, type_data = NULL, folder_name) {
  # Charger toutes les données
  data_all <- get_data(folder_name)
  
  # Vérifier si le mois est valide
  if (!month %in% names(data_all)) {
    stop("Error: Invalid month. Valid options are: ", paste(names(data_all), collapse = ", "))
  }
  
  # Extraire les données pour le mois spécifié
  month_data <- data_all[[month]]
  
  # Si aucun type de données n'est spécifié, retourner les données du mois
  if (is.null(type_data)) {
    return(month_data)
  }
  
  # Vérifier si le type de données est valide
  if (!type_data %in% names(month_data)) {
    stop("Error: Invalid type_data. Valid options are: ", paste(names(month_data), collapse = ", "))
  }
  
  # Retourner les données spécifiques
  return(month_data[[type_data]])
}


# Fonction pour afficher le head de chaque mois
show_head_per_month <- function(data_list, n = 6) {
  for (month in names(data_list)) {
    cat("\nPremières", n, "lignes pour le mois :", month, "\n")
    print(head(data_list[[month]], n)) # Affiche les n premières lignes
  }
}


# Fonction pour afficher les types de données des colonnes pour chaque mois
check_column_types <- function(data_list) {
  for (month in names(data_list)) {
    cat("\nTypes de données pour le mois :", month, "\n")
    print(sapply(data_list[[month]], class)) # Affiche les types de colonnes
  }
}


########## netoyage de données #################

# Fonction pour vérifier les valeurs manquantes dans chaque mois
check_missing_values <- function(data_list) {
  # Initialiser une liste pour stocker les résultats
  missing_summary <- list()
  
  # Parcourir chaque mois et calculer les valeurs manquantes
  for (month in names(data_list)) {
    data <- data_list[[month]]
    missing_counts <- colSums(is.na(data)) # Compter les valeurs manquantes par colonne
    total_missing <- sum(missing_counts)  # Total des valeurs manquantes pour le mois
    missing_summary[[month]] <- list(
      missing_counts = missing_counts,
      total_missing = total_missing
    )
  }
  
  return(missing_summary)
}

# Fonction pour vérifier le format de la colonne Date
check_date_format <- function(data_list) {
  for (month in names(data_list)) {
    data <- data_list[[month]]
    
    # Convertir la colonne Date/Time en format Date
    data$DateTime <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S", tz="UTC")
    
    # Vérifier les lignes avec des dates invalides
    invalid_dates <- data[is.na(data$DateTime), ]
    
    # Afficher un message si des dates invalides sont trouvées
    if (nrow(invalid_dates) > 0) {
      cat("\nDates invalides trouvées pour le mois :", month, "\n")
      print(invalid_dates)
    } else {
      cat("\nToutes les dates sont valides pour le mois :", month, "\n")
    }
  }
}

# Charger toutes les données
folder_name <- "C:/Users/ADMIN/Documents/ECE/Analyse de données/projet_noté/UberDatasets" # Remplacez par le chemin de votre dossier
data_list <- get_data(folder_name)


# Vérifier les types de colonnes pour chaque mois
check_column_types(data_list)


# avoir un appercus de chaque mois

show_head_per_month(data_list, n = 6)

# Vérifier les valeurs manquantes
missing_data_summary <- check_missing_values(data_list)

# Afficher le résumé des valeurs manquantes
for (month in names(missing_data_summary)) {
  cat("\nValeurs manquantes pour le mois :", month, "\n")
  print(missing_data_summary[[month]])
}

# vérification du formarmat de la date
check_date_format(data_list)

###### combinaison de toute les données en un seul dataframe ###########

all_data <- bind_rows(data_list, .id = "month")

# conversion de la colonnes date 
all_data$DateTime <- mdy_hms(all_data$Date.Time)

#ajout d'une colonnes jour et de mois
all_data <- all_data %>%
  mutate(day = date(DateTime),   # Jour
         month = floor_date(DateTime, "month"))  # Mois

#Calculer le Nombre Quotidien de Trajets
daily_trips <- all_data %>%
  group_by(day) %>%
  summarise(num_trips = n())

#Calculer le Nombre Mensuel de Trajet
monthly_trips <- all_data %>%
  group_by(month) %>%
  summarise(num_trips = n())

# Visualiser les Résultats
# Graphique du nombre quotidien de trajets
ggplot(daily_trips, aes(x = day, y = num_trips)) +
  geom_line() +
  labs(title = "Nombre Quotidien de Trajets",
       x = "Jour",
       y = "Nombre de Trajets") +
  theme_minimal()

# Graphique du nombre mensuel de trajets
ggplot(monthly_trips, aes(x = month, y = num_trips)) +
  geom_bar(stat = "identity") +
  labs(title = "Nombre Mensuel de Trajets",
       x = "Mois",
       y = "Nombre de Trajets") +
  theme_minimal()

########## Analyse des Heures de Pointe ###########
# extraction des heures de la colonnes DAte/Times pour crées une nouvelle colonnes Heure
all_data <- all_data %>%
  mutate(hour = hour(DateTime))  # Extraire l'heure

#Calcule du Nombre de Trajets Par Heure

hourly_trips <- all_data %>%
  group_by(hour) %>%
  summarise(num_trips = n())

#Visualisation des Heures de Pointe
ggplot(hourly_trips, aes(x = hour, y = num_trips)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Nombre de Trajets par Heure",
       x = "Heure de la Journée",
       y = "Nombre de Trajets") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23)  # Afficher toutes les heures

###### Analyse des Zones Géographiques ##########

# vérification des colonnes latitude et longitude
summary(all_data$Lat)
summary(all_data$Lon)

# visualisation des trajets sur une carte
# Créer un graphique de dispersion des trajets
ggplot(all_data, aes(x = Lon, y = Lat)) +
  geom_point(alpha = 0.1, color = "blue") +  # Ajustez alpha pour la transparence
  labs(title = "Trajets Uber - Répartition Géographique",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  coord_fixed(ratio = 1)  # Conserver le ratio pour la carte

# superposition des trajet sur une cartes
# Obtenir une carte de New York (ou votre zone d'intérêt)
ny_map <- get_map(location = c(lon = -73.9857, lat = 40.7484), zoom = 12)

# Tracer les trajets sur la carte
ggmap(ny_map) +
  geom_point(data = all_data, aes(x = Lon, y = Lat), alpha = 0.1, color = "blue") +
  labs(title = "Trajets Uber sur la Carte de New York",
       x = "Longitude",
       y = "Latitude")




# calcul du nombre moyen de passager par jour
# calcul du nombre de passager par jour 
daily_passengers <- all_data %>%
  group_by(day) %>%
  summarise(num_passengers = n())  # Compte le nombre de trajets par jour

avg_passengers <- mean(daily_passengers$num_passengers)
print(paste("Le nombre moyen quotidien de passagers est :", avg_passengers))
