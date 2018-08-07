#' @title Data Reader
#'
#' @description A function that reads the raw tab
#'  separated input from the file /raw_data/input
#'
#' @importFrom magrittr %<>% %>%
#'
#' @export


data_reader <- function() {
  return(
    getwd() %>%
      paste0("/raw_data/input") %>%
      read.csv2(
        header = TRUE,
        sep = "\t",
        stringsAsFactors = FALSE
      )
  )
}
