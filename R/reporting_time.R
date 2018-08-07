#' @title Reporting Time
#'
#' @description A function that takes a map.data data frame
#'  and creates boxplots of turnaround times for each particular
#'  US state.
#'
#' @param map.data A data frame containing the full data
#'  set, with a state column appended on.
#'
#' @export


reporting_time <- function(map.data) {

  # Get unique states again (state by state basis)
  uniqueStates <- map.data$state %>%
    pdata::get_uni_states()

  # Loop over and filter all states and create unique data frames
  finalTats <- lapply(
    X = uniqueStates,
    FUN = function(x) {
      state.data <- map.data %>% subset(map.data$state %>% `==`(x))
      allTats <- state.data$Tat
      invalidTats <- allTats %>% is.na %>% `|`(allTats %>% `<`(0))
      if (invalidTats %>% any) allTats %<>% `[`(invalidTats %>% `!`())

      data.frame(
        state = x,
        tats = allTats,
        stringsAsFactors = FALSE
      )
    }
  ) %>%
    purrr::reduce(rbind)

  # Create an original plot and zoom one in
  g <- ggplot2::ggplot(
    data = finalTats,
    mapping = ggplot2::aes(
      x = factor(state),
      y = tats
    )
  ) %>%
    `+`(
      ggplot2::geom_boxplot()
    ) %>%
    `+`(
      ggplot2::ggtitle("Turnaround times per US State")
    ) %>%
    `+`(
      ggplot2::xlab("US State")
    ) %>%
    `+`(
      ggplot2::ylab("Turnaround time in days")
    ) %>%
    `+`(
      pdata::plot_theme()
    ) %>%
    `+`(
      ggplot2::ylim(c(0, 20))
    ) %>%
    `+`(
      ggplot2::stat_summary(
        fun.y = mean,
        geom = "line",
        ggplot2::aes(
          colour = 'red',
          group = 1
        )
      )
    ) %>%
    `+`(
      ggplot2::theme(
        legend.title = ggplot2::element_blank()
      )
    ) %>%
    `+`(
      ggplot2::scale_colour_discrete(
        labels = "mean"
      )
    )

  ggplot2::ggsave(
    filename = getwd() %>% paste0("/imgs/tat_box.png"),
    plot = g
  )

  return(g)
}
