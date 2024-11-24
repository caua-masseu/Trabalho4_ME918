load_initial_data <- function(file_path) {
  data <- read_xlsx(file_path, skip = 6)
  colnames(data) <- c("Date", "Cloudiness", "Rainfall", "Max_Temperature", "Min_Temperature", "Humidity", "Mean_Temperature")
  data$Date <- as.Date(data$Date)
  return(data)
}
