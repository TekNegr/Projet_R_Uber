#######################################"DEFINITION"#########################################
define_proj<-function(){
  install.packages(c("dplyr", "ggplot2", "lubridate", "httr", "tidyr","maps", "ggmap","jsonlite","lubridate", "magrittr"))
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(maps)
  library(ggmap)
  library(httr)
  library(jsonlite)
  library(lubridate)
  library(magrittr)
}
#latitude,longitude,elevation,utc_offset_seconds,timezone,timezone_abbreviation
#40.738136,-74.04254,51.0,-18000,America/New_York,EST

define_proj()


#######################################"CODE"#########################################


get_data <- function(folder_name){
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
  

show_head_per_month <- function(data_list, n = 6) {
  for (month in names(data_list)) {
    cat("\nPremières", n, "lignes pour le mois :", month, "\n")
    print(head(data_list[[month]], n)) # Affiche les n premières lignes
  }
}


get_Weather<-function(){
  if(file.exists("Data/open-meteo.csv")){
    print("Getting Weather REAL data..")
    data_weather<-read.csv("Data/open-meteo.csv", sep=",")
    return(data_weather)
    
  }
  else{
    print("Download 'open-meteo.csv' gros Baka >_<'")
    return(NULL)
  }
}


clean_weather<-function(weather_data){
  
  if (is.null(weather_data)){
    print("You Need Weather Data to clean ,':/")
    return(NULL)
  }
  else{
    numeric_cols <- sapply(weather_data, is.numeric)
    weather_data[numeric_cols] <- lapply(weather_data[numeric_cols], function(x) {
      ifelse(is.na(x), mean(x, na.rm = TRUE), x)
    })
    
    categorical_cols <- sapply(weather_data, is.character)
    weather_data[categorical_cols] <- lapply(weather_data[categorical_cols], function(x) {
      x[is.na(x)] <- "Unknown"
      return(x)
    })
    return(weather_data)
  }
}


clean_data<-function(){}


check_missing_data<-function(w_data){
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


check_column_types<-function(data_list){
  for (month in names(data_list)) {
    cat("\nTypes de données pour le mois :", month, "\n")
    print(sapply(data_list[[month]], class)) # Affiche les types de colonnes
  }
}


get_Wavg<-function(w_data){
  if(is.null(w_data)){
    print("Need a weather to use this function Baka ;p")
    return(NULL)
  }
  else{
    avg[[w_data$
      w_data$
    return avg
  }
}


Get_Wspec_monthly<-function(spec_type, w_data){
  if (spec_type=="Clouds"){
    return 
  }
  else if (spec_type=="pressure"){}
  else if (spec_type=="rain"){}
  else if (spec_type=="humidity"){}
  else if (spec_type=="wind"){}
  
}

get_correlation(data1, data2){
  
}

get_plot(plot_type, data_type, spec_type){
  
}

summary_plot <- function(data){
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }
  summary_table <- data %>%
    select(where(is.numeric)) %>%
    summarise(across(
      everything(),
      list(
        Min = ~ min(.),
        `1st Qu.` = ~ quantile(., 0.25),
        Median = ~ median(.),
        Mean = ~ mean(.),
        `3rd Qu.` = ~ quantile(., 0.75),
        Max = ~ max(.)
      )
    )) %>%
    pivot_longer(cols = everything(), names_to = "Variable_Statistic", values_to = "Value") %>%
    separate(Variable_Statistic, into = c("Variable", "Statistic"), sep = "_", extra = "merge") %>%
    mutate(Value = round(Value, 2)) # Round values for readability
  
  # Convert data back to wide format for proper display
  summary_table_wide <- summary_table %>%
    pivot_wider(names_from = Statistic, values_from = Value)
  
  # Create a table-like plot
  description_plot <- ggplot(summary_table, aes(x = Variable, y = Statistic)) +
    geom_tile(fill = "white", color = "black") +
    geom_text(aes(label = Value), size = 3.5) +
    labs(
      title = "Dataset Description",
      x = "Variables",
      y = "Statistics"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14)
    )
  return(description_plot)
}

space<-function(){print("-------------X----------------X------------------- ")}

#####################################"LAUNCHER"#######################################
launch<-function(){
  w_data<-get_Weather()
  space()
  print("Getting original missing data")
  print(check_missing_data(w_data))
  w_clean_data<-clean_weather(w_data)
  print(summary_plot(w_clean_data))
  space()
  space()
  print("Head")
  print(head(w_clean_data))
  space()
  space()
  print("Footer")
  print(tail(w_clean_data))

}

launch()
#END