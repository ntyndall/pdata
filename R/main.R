

main <- function() {

  # Load the data set
  my.data <- pdata::data_reader()

  # Get some map data...
  map.data <- my.data %>%
    subset(my.data$Client.Zip %in% c("", "Moroc") %>% `!`())

  # Convert ZIP codes to integers
  map.data$Client.Zip %<>% as.integer

  # Get ZIP code hash and create map
  mapInfo <- map.data$Client.Zip %>%
    pdata::create_map()

  # Append actual states to the main data set
  map.data$state <- sapply(
    X = map.data$Client.Zip,
    FUN = function(x) x %>% mapInfo$find()
  )

  # Calculate turnaround time
  map.data %>%
    pdata::reporting_time()




}
