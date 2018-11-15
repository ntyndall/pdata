#' @title Method Plot
#'
#' @description A function that takes a map.data data frame
#'  and creates stacked bar chart statistics of the type
#'  of methodology used in map.data.
#'
#' @param map.data A data frame containing the full data
#'  set, with a state column appended on.
#'
#' @export


method_plot <- function(map.data) {

  # Get all types of methods
  allMethods <- map.data$Methodology %>%
    table %>%
    data.frame

  # Look at minor methodologies
  investigateMethods <- allMethods[ ,1] %>%
    as.character %>%
    setdiff("IHC")

  # Create data frames and counts for bar chart
  uniqStates <- map.data$state %>%
    us.mapper::get_uni_states()
  methodCounts <- lapply(
    X = uniqStates,
    FUN = function(x) {
      single <- map.data %>%
        subset(map.data$state %>% `==`(x) & map.data$Methodology %in% investigateMethods)

      if (single %>% nrow %>% `>`(0)) {
        c(
          single %>% nrow,
          data.frame(
            state = x,
            method = single$Methodology,
            stringsAsFactors = FALSE
          ) %>%
            list
        )
      } else {
        c(0, NULL)
      }
    }
  )

  # Only return the top ten states by count of methodologies
  topTen <- methodCounts %>%
    purrr::map(1) %>%
    purrr::flatten_dbl() %>%
    order(decreasing = TRUE) %>%
    `[`(1:10)

  # Create the long data frame for plotting
  forPlotting <- methodCounts %>%
    `[`(topTen) %>%
    purrr::map(2) %>%
    purrr::reduce(rbind)

  g <- ggplot2::ggplot(
    data = forPlotting,
    mapping = ggplot2::aes(state)
  ) %>%
    `+`(
      ggplot2::geom_bar(
        ggplot2::aes(
          fill = method
        ),
        colour = "black"
      )
    ) %>%
    `+`(
      pdata::plot_theme()
    ) %>%
    `+`(
      ggplot2::ggtitle("Top Methodologies for Ten US States")
    )

  # Return the plot
  return(g)
}
