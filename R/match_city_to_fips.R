#' @title Match City To FIPS
#' 
#' @export


match_city_to_fips <- function(data.set, cityCol = "City", stateName) {

  # Get FIPs code based on state
  single.state <- stateName %>% 
    subset_state()

  # Append FIPs on where possible
  data.set$fips <- lapply(
    X = data.set %>% `[[`(cityCol),
    FUN = function(x) {
      return(
        if (x %in% single.state$city) {
          single.state$fips[x %>% `==`(single.state$city) %>% which %>% `[`(1)]
        } else {
          0
        }
      )
    }
  ) %>% 
    purrr::flatten_dbl()
  
  # Return updated data set back
  return(data.set)
}