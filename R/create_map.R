


create_map <- function(myZipcodes) {
  # Load zipcode data in
  data(zipcode, package = "zipcode")
  data(df_pop_state, package = "choroplethr")
  possibleStates <- df_pop_state$region
  zipcode$zip %<>% as.integer

  # Unique zip codes in data set
  uniqZip <- myZipcodes %>%
    unique

  # Figure out actual state form zip code for UNIQUE ZIPs only
  myStates <- lapply(
    X = uniqZip,
    FUN = function(x) {
      zipMatch <- x %>% `==`(zipcode$zip)
      res <- if (zipMatch %>% any) {
        single <- zipcode %>%
          subset(zipMatch) %>%
          `[`("state") %>%
          as.character %>%
          openintro::abbr2state()
      } else {
        ""
      }
      if (res %>% is.na) "" else res
    }
  ) %>%
    lapply(tolower) %>%
    purrr::flatten_chr()

  # Do any not belong here?
  doNotBelong <- myStates %in% possibleStates %>% `!`()
  if (doNotBelong %>% any) {
    myStates[doNotBelong] <- "unknown"
  }

  # Match with uniques and get ACTUAL counts!
  uniqStates <- myStates %>% unique
  totalCounts <- sapply(
    X = 1:(uniqStates %>% length),
    FUN = function(x) {
      uniqZip %>%
        `[`(uniqStates[x] %>% `==`(myStates)) %>%
        sapply(function(y) y %>% `==`(myZipcodes) %>% sum) %>%
        sum
    }
  )

  # Create look up hash
  zipLookup <- hashmap::hashmap(
    keys = uniqZip,
    values = myStates
  )

  # Set up data frame of counts
  stateFreq <- data.frame(
    region = uniqStates,
    value = totalCounts,
    stringsAsFactors = FALSE
  )

  # Any states missing?
  missingStates <- possibleStates %in% stateFreq$region %>% `!`()
  if (missingStates %>% any) {
    missing <- possibleStates[missingStates]
    stateFreq %<>% rbind(
      data.frame(
        region = missing,
        value = 0 %>% rep(missing %>% length),
        stringsAsFactors = FALSE
      )
    )
  }

  # And finally, take out any states, such as unknown that won't plot nicely.
  tooMany <- stateFreq$region %in% possibleStates %>% `!`()
  if (tooMany %>% any) stateFreq %<>% subset(tooMany %>% `!`())

  # Make sure to sort the data frame, then add on state populations and normalise
  stateFreq <- stateFreq[stateFreq$region %>% order, ]
  stateFreq$populations <- df_pop_state$value

  allResults <- lapply(
    X = 1:2,
    FUN = function(x) {
      # First do absolute, then normalised with population
      if (x == 2) stateFreq$value %<>% `/`(stateFreq$populations) %>%  `*`(1000)

      # Plot the results
      singlePlot <- stateFreq %>%
        choroplethr::state_choropleth(
          title = "Test counts " %>% paste0(if (x == 1) "(Absolute)" else "(Normalized / 1000 people)"),
          legend = 'Tests'
        ) %>%
        `+`(
          pdata::map_theme(titleFont = 16)
        ) %>%
        `+`(
          ggplot2::scale_fill_brewer(
            palette = if (x == 1) 1 else 13
          )
        )

      # Save the figures
      ggplot2::ggsave(
        filename = getwd() %>% paste0("/imgs/worldmap", x, ".png"),
        plot = singlePlot
      )

      return(
        list(
          plot = singlePlot,
          data = stateFreq[stateFreq$value %>% order(decreasing = TRUE) %>% `[`(1:5), ]
        )
      )
    }
  )

  # Create image glob
  gridPlots <- suppressWarnings(
    gridExtra::grid.arrange(
      allResults[[1]]$plot,
      allResults[[2]]$plot,
      nrow = 2
    )
  )

  # Return the hash map back to append onto the main data set
  return(zipLookup)
}

