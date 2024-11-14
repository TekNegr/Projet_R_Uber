#######FONCTION POUR RECUPERER TOUTE LA DATA
get_data <- function(){
  
  months <- c("apr14", "aug14", "jul14", "jun14", "may14", "sep14")
  file_paths <- file.path("dataset", paste0("uber-raw-data-", months, ".csv"))
  
  data_list <- lapply(file_paths, read.csv)
  
  names(data_list) <- months
  
  return(data_list)

}


#######FONCTION POUR RECUPERER UN MOIS OU UNE DATA SPECIFIC
get_spec_data<-function(month, type_data){
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


#####FONCTION POUR NETTOYER LA DONNEE
clean

######FONCTION POUR AFFICHER UN SCHEMA


#####FONCTION POUR

