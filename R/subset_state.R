#' @title Subset State
#'
#' @export


subset_state <- function(stateName) {
  # Load in data sests from noncensus
  data(zip_codes, package = "noncensus")
  
  # Get the state abbreviation
  stateAbbr <- currentState %>%
    openintro::state2abbr()
  
  # Only return over state of interest
  return(
    zip_codes %>% 
      subset(zip_codes$state %>% `==`(stateAbbr))
  )
}