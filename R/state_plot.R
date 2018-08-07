#' @title State Plot
#'
#' @description A function that takes a map.data data frame
#'  and creates county specific metrics on a state by
#'  state basis, depending on the input provided.
#'
#' @param map.data A data frame containing the full data
#'  set, with a state column appended on.
#' @param currentState A character string that has to be
#'  one of the 50 US states.
#'
#' @export


state_plot <- function(map.data, currentState = "california") {

  # Get a single state
  single.state <- map.data %>%
    subset(map.data$state %>% `==`(currentState))

  # Get the state abbreviation
  stateAbbr <- currentState %>%
    openintro::state2abbr()

  # Load zip codes and county names from nonensus
  data(zip_codes, package = "noncensus")
  data(counties, package = "noncensus")

  stateZips <- zip_codes %>%
    subset(zip_codes$state %>% `==`(stateAbbr))

  counties %<>% subset(counties$state %>% `==`(stateAbbr))

  # Need to create FIPs = state_fips * 1000 + county_fips
  counties$fips <- counties$state_fips %>%
    as.character %>%
    as.numeric %>%
    `*`(1000) %>%
    `+`(
      counties$county_fips %>%
        as.character %>%
        as.numeric
    )

  # Calculate FIPS for every row of a particular state
  allFips <- lapply(
    X = single.state$Client.Zip,
    FUN = function(x) stateZips %>% subset(x %>% `==`(stateZips$zip)) %>% `[`("fips") %>% as.integer
  ) %>%
    purrr::flatten_dbl()

  # Bind the FIPs on, so I can subset elements from the plot
  single.state$fips <- allFips

  # Create basic data and append missing regions.
  fipFrame <- allFips %>%
    pdata::create_fip_frame() %>%
    pdata::missing_states(
      stateZips = stateZips
    )

  # Define by hand the colour palette
  colorSchemes <- list(
    blue = 1,
    green = 2,
    grey = 6,
    orange = 7
  )

  # Unique antibodies, remove special characters for PDF
  uniqA <- single.state$Antibody %>% table
  types <- names(uniqA)

  # Remove tests with no antibodies trialed
  emptyTrial <- types %>% `==`("")
  if (emptyTrial %>% any) types %<>% `[`(emptyTrial %>% `!`())

  # Create 4 plots
  allResults <- lapply(
    X = 1:(types %>% length),
    FUN = function(x) {
      # Do subsetting here first
      sub.state <- single.state %>%
        subset(
          single.state$Antibody %>% `==`(types[x])
        )

      # Create sub data for a particular antibody type
      dataToPlot <- sub.state$fips %>%
        pdata::create_fip_frame() %>%
        pdata::missing_states(
          stateZips = stateZips
        )

      # Remove special characters for title for the PDF
      ty <- types[x] %>%
        stringr::str_replace(pattern = '[[:punct:]]+', '')

      # Plot the data
      singlePlot <- dataToPlot %>%
        choroplethr::county_choropleth(
          state_zoom = currentState,
          title = paste0("Antibody : ", ty)
        ) %>%
        `+`(
          ggplot2::scale_fill_brewer(
            palette = colorSchemes[[x]]
          )
        ) %>%
        `+`(
          pdata::map_theme(titleFont = 16)
        )

      # Get the top fips of the data set and convert back to a county..
      topResults <- dataToPlot[dataToPlot$value %>% order %>% rev %>% `[`(1:4), ]
      topResults$county <- sapply(
        X = topResults$region,
        FUN = function(y) counties$county_name %>% `[`(y %>% `==`(counties$fips))
      )

      # Return the results
      return(
        list(
          plot = singlePlot,
          results = topResults
        )
      )
    }
  )

  # Convert the top results to a list of data frames
  resultData <- allResults %>% purrr::map(2)
  names(resultData) <- types

  # Arrange the plots
  gridPlots <- suppressWarnings(
    gridExtra::grid.arrange(
      allResults[[1]]$plot,
      allResults[[2]]$plot,
      allResults[[3]]$plot,
      allResults[[4]]$plot,
      ncol = 2,
      nrow = 2
    )
  )

  # Return all data back
  return(resultData)
}

#' @title Missing States
#'
#' @description A function that calculates the missing
#'  FIP codes and initializes them to 0 to cover every
#'  county in the state.
#'
#' @param allFips A data frame containing county and
#'  fip code data.
#' @param stateZips A data frame containing both US zip
#'  and fip codes.
#'
#' @export


missing_states <- function(allFips, stateZips) {
  # Check for missing regions.
  uniqueFips <- stateZips$fips %>% unique
  containedFips <- uniqueFips %in% allFips$region %>% `!`()

  # Append zero's if any FIPs are missing
  if (containedFips %>% any) {
    zerodFips <- uniqueFips %>% `[`(containedFips)
    allFips %<>% rbind(
      data.frame(
        region = zerodFips,
        value = 0,
        stringsAsFactors = FALSE
      )
    )
  }

  # Return the possibly zero appended data frame
  return(allFips)
}

#' @title Create FIP Frame
#'
#' @description A function that transforms a data frame
#'  into one that is used by choroplethr.
#'
#' @param fipData A data frame with two columns that has
#'  the FIP values and corresponding frequency counts.
#'
#' @export


create_fip_frame <- function(fipData) {
  fipData %<>% table %>% data.frame
  names(fipData) <- c("region", "value")
  fipData$region %<>% as.character %>% as.integer

  # Return new data frame back
  return(fipData)
}
