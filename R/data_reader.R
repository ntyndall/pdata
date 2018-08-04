#' @title Data Reader
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
