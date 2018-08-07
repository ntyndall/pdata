#' @title Plot Theme
#'
#' @description Holds the theme for the plots to be used
#'  with ggplot.
#'
#' @param fontFamily A character string that defines the font
#'  family to be used for the axis / title / legend etc.
#'  Default to _Gotham_
#' @param titleFont An integer value that defines the title
#'  font size.
#'
#' @export


plot_theme <- function(fontFamily = 'Gotham', titleFont = 16) {
  return(
    ggplot2::theme(
      plot.title =  ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = '#353535',
        size = titleFont,
        hjust = 0.5
      ),
      legend.title = ggplot2::element_text(
        family = fontFamily,
        face =  'plain',
        colour = '#353535',
        size = 14
      ),
      legend.text = ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = "#353535",
        size = 12
      ),
      legend.key = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        family = fontFamily,
        face =  'plain',
        colour = '#353535',
        size = 12,
        angle = 90,
        vjust = 0.5
      ),
      axis.text.y = ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = '#353535',
        size = 12
      ),
      panel.border = ggplot2::element_rect(
        linetype = "dashed",
        colour = '#353535',
        fill = NA
      ),
      text = ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = '#353535',
        size = 14
      ),
      panel.background = ggplot2::element_rect(
        fill = 'white'
      ),
      plot.background = ggplot2::element_rect(
        fill = "white"
      ),
      legend.background = ggplot2::element_rect(
        fill = "white",
        size = 0.5
      ),
      panel.grid.major = ggplot2::element_line(
        colour = "#353535",
        size = 0.1,
        linetype = 'dotted'
      ),
      panel.grid.minor = ggplot2::element_line(
        colour = "#353535",
        size = 0.1,
        linetype = 'dotted'
      )
    )
  )
}

#' @title Map Theme
#'
#' @description Holds the theme for the plots using
#'  ggplot choroplethr images.
#'
#' @param fontFamily A character string that defines the font
#'  family to be used for the axis / title / legend etc.
#'  Default to _Gotham_.
#' @param titleFont An integer value that defines the title
#'  font size.
#'
#' @export


map_theme <- function(fontFamily = 'Gotham', titleFont = 26) {
  return(
    ggplot2::theme(
      plot.title =  ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = '#353535',
        size = titleFont,
        hjust = 0.5
      ),
      legend.title = ggplot2::element_text(
        family = fontFamily,
        face =  'plain',
        colour = '#353535',
        size = 12
      ),
      legend.text = ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = "#353535",
        size = 10
      )
    )
  )
}
