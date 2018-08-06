#' @title Ratio Features
#'
#' @export


ratio_features <- function(map.data) {

  antibodyType <- map.data$Antibody %>% unique %>% setdiff("")
  uniqStates <- map.data$state %>% unique
  pToT <- lapply(
    X = uniqStates,
    FUN = function(x) {

      # Get subset of data
      sub.data <- map.data %>% subset(map.data$state %>% `==`(x))
      testLen <- sub.data %>% nrow

      # Ratio of patients to tests
      ptt <- (sub.data$Patient.Id %>% unique %>% length) %>%
        `/`(testLen)

      # Ratio of cancellations to orders
      cto <- sub.data$Status %>%
        `==`("Cancelled") %>%
        sum %>%
        `/`(testLen)

      # Ratio of antibodies per state
      antiRes <- sapply(
        X = antibodyType,
        FUN = function(y) {
          subRes <- sub.data %>% subset(sub.data$Antibody %>% `==`(y))
          subRes$Antibody %>% length
        }
      ) %>%
        as.integer %>%
        `/`(testLen)

      # Return
      c(ptt, cto, antiRes)
    }
  )

  # Stack all ratios for ggplot
  types <- c(1:(pToT[[1]] %>% length))
  ratios <- lapply(
    X = 1:(types %>% length),
    FUN = function(x) {
      data.frame(
        uniqueState = uniqStates,
        patientToTest = pToT %>% purrr::map(x) %>% purrr::flatten_dbl(),
        type = types[x],
        stringsAsFactors = FALSE
      )
    }
  ) %>%
    purrr::reduce(rbind)

  # Define new labels
  newLabels <- c("Patients", "Cancelled", antibodyType)

  g <- suppressWarnings(
    ggplot2::ggplot(
      data = ratios,
      mapping = ggplot2::aes(
        x = factor(type),
        y = patientToTest,
        fill = factor(type)
      )
    ) %>%
      `+`(
        ggplot2::geom_boxplot()
      ) %>%
      `+`(
        ggplot2::ggtitle("Ratios per US State")
      ) %>%
      `+`(
        ggplot2::xlab("")
      ) %>%
      `+`(
        ggplot2::ylab("Ratios")
      ) %>%
      `+`(
        pdata::plot_theme()
      ) %>%
      `+`(
       ggplot2::theme(
         axis.text.y = ggplot2::element_blank(),
         axis.ticks.y = ggplot2::element_blank()
        )
      ) %>%
      `+`(
        ggplot2::scale_fill_manual(
          values = c("#aa182b", "#fff275", "#f59f06", "#a3d9ff", "#81f4e1", "#f3fefc"),
          name = "Legend",
          labels = newLabels
        )
      ) %>%
      `+`(
        ggplot2::coord_flip()
      )
  )

  # Save a copy of the plot
  ggplot2::ggsave(
    filename = getwd() %>% paste0("/imgs/ratios.png"),
    plot = g
  )

  # Split ratios back up and order them (take the top few)
  newRatios <- lapply(
    X = 1:6,
    FUN = function(x) {
      newSet <- ratios %>% subset(ratios$type %>% `==`(x))
      vals <- newSet$patientToTest %>% order %>% rev
      newSet[c(
        vals %>% utils::head(2),
        vals %>% `[`(vals %>% length %>% `/`(2) %>% floor),
        vals %>% utils::tail(2)), ]
    }
  )
  names(newRatios) <- newLabels

  # Return data back, including the plot
  return(
    list(
      data = newRatios,
      plot = g
    )
  )
}
