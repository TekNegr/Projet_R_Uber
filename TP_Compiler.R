##############################"DEFINITION"###################################
define_proj<-function(){
  packages_bundle = c("dplyr", "ggplot2", "lubridate","maps", "ggmap","lubridate", "magrittr","caret", "randomForest", "xgboost", "glmnet")
  
  for(name in packages_bundle){
    if(!require(name, character.only = TRUE)){
      install.package(name)
    }
    library(name, character.only = TRUE)
  }
  
}

##############################"DATASET METEO"###################################

Wget_data<-function(){
  print("Getting Weather")
}

get_Weather_spec<-function(Weather_Type = "Wind", Fork = "Monthly", W_data){}

get_Weather_plot<-function(Weather_Type = "Wind", Plot_type = "Bar",Fork = "Monthly", W_data){} 

get_<-function(){}

WClean_data<-function(W_data){}

##############################"DATASET UBER"###################################


Uget_data<-function(folder_name = "data"){
  print("Getting Uber Data...")
  months <- c("apr14", "aug14", "jul14", "jun14", "may14", "sep14")
  file_paths <- file.path(folder_name, paste0( "uber-raw-data-", months, ".csv"))
  
  data_list <- lapply(file_paths, function(path) {
    if (file.exists(path)) {
      read.csv(path)
    } else {
      message("File not found: ", path)
      return(NULL)
    }
  })
  
  names(data_list) <- months
  
  return(data_list)
}

get_Trips<-function(Fork = "Monthly", U_data){
  
  
}

get_Trip_plot<-function(Plot_type = "Bar",Fork = "Monthly", U_data){}

UClean_data<-function(U_data){
  all_data <- bind_rows(data_list, .id = "month")
  all_data$DateTime <- mdy_hms(all_data$Date.Time)
  all_data <- all_data %>%
    mutate(day = date(DateTime),
           month = floor_date(DateTime, "month"))
    
  if (is.null(U_data)){
    print("You Need Weather Data to clean ,':/")
    return(NULL)
  }
  else{
    numeric_cols <- sapply(U_data, is.numeric)
    weather_data[numeric_cols] <- lapply(U_data[numeric_cols], function(x) {
      ifelse(is.na(x), mean(x, na.rm = TRUE), x)
    })
    
    categorical_cols <- sapply(U_data, is.character)
    weather_data[categorical_cols] <- lapply(U_data[categorical_cols], function(x) {
      x[is.na(x)] <- "Unknown"
      return(x)
    })
    return(weather_data)
  }
  
}

#########################"MANIPULATEUR COMMUNS"################################

space<-function(text_dis = " "){
  print(".                                                 .")
  print(".              .",text_dis,".                     .")
  print(".                                                 .")
}

merge_datasets<-function(U_data, W_data){}

get_Monthly_Head<-function(data_list, n = 6){
  for (month in names(data_list)) {
    cat("\nPremières", n, "lignes pour le mois :", month, "\n")
    print(head(data_list[[month]], n)) # Affiche les n premières lignes
  }                                          
}

get_Missing_data<-function(data_list){
  missing_summary <- data.frame(Column = character(),
                                Missing_Count = integer(),
                                stringsAsFactors = FALSE)
  for (col_name in names(w_data)) {
    missing_count <- sum(is.na(w_data[[col_name]]))
    missing_summary <- rbind(missing_summary, 
                             data.frame(Column = col_name, 
                                        Missing_Count = missing_count, 
                                        stringsAsFactors = FALSE))
  }
  return(missing_summary)

}

get_Clean_data<-function(){}

get_date_formatted<-function(data_list){
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

get_column_types<-function(data_list){
  for (month in names(data_list)) {
    cat("\nTypes de données pour le mois :", month, "\n")
    print(sapply(data_list[[month]], class)) # Affiche les types de colonnes
  }
}

get_Merged_Plot<-function(Plot_type="Line",All_data ){}

get_Random_Plot_Theme<-function(){}

get_Binary_Classification<-function(){}
  
get_Split_data<-function(All_data){}
##############################"LAUNCHER"###################################
Launch_<-function(){
  define_proj()
  W_data <- Wclean_data(Wget_data())
  U_data <- Uclean_data(Uget_data())
  All_data <-merge_datasets(U_data, W_data)
  get_Monthly_Head(W_data)
  get_Monthly_Head(U_data)
  get_Monthly_Head(All_data)
  
  # get_Trip_plot(Plot_type = "Bar", Fork = "Monthly", U_data)
  
  # get_Trip_plot(Plot_type = "Line", Fork = "Monthly", U_data)
  # get_Trip_plot(Plot_type = "Line", Fork = "Monthly", U_data)
  # get_Trip_plot(Plot_type = "Line", Fork = "Daily", U_data)
  # get_Trip_plot(Plot_type = "Line", Fork = "Hourly", U_data)
  
  # get_Trip_plot(Plot_type = "Map", Fork = NULL , U_data)
  # get_Trip_plot(Plot_type = "Points", Fork = NULL, U_data)
  
  # get_Weather_plot<-function(Weather_Type = "Wind", Plot_type = "Bar",W_data,Fork = "Monthly")
  # get_Weather_plot<-function(Weather_Type = "Precipitation", Plot_type = "Bar",W_data,Fork = "Monthly")
  # get_Weather_plot<-function(Weather_Type = "Sun", Plot_type = "Bar",W_data,Fork = "Monthly")
  # get_Weather_plot<-function(Weather_Type = "Humidity", Plot_type = "Bar",W_data,Fork = "Monthly")
  
  # get_Merged_Plot(Weather_type="Wind", U_data, W_data )
  # get_Merged_Plot(Weather_type="Precipitation", U_data, W_data )
  # get_Merged_Plot(Weather_type="Sun", U_data, W_data )
  # get_Merged_Plot(Weather_type="Humidity", U_data, W_data )
  
}

Launch_()
