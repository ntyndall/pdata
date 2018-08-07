#' @title Get Unique States
#'
#' @description A function that takes a vector of multiple
#'  states and calculates the unique possibilities, while
#'  filtering out states that don't comply.
#'
#' @param allStates A character vector of (possible) US
#'  states.
#'
#' @export


get_uni_states <- function(allStates) {

  # Get unique states from the data set
  uniqueStates <- allStates %>% unique

  # Return valid states
  return(
    uniqueStates %>%
      `[`(uniqueStates %>% `!=`("unknown"))
  )
}
