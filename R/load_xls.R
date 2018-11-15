#' @title Load XLS
#'
#' @export


load_xls <- function(loc) {

  data.set <- gdata::read.xls(
    xls = loc,
    sheet = 1, 
    header = F,
    pattern = "City",  
    stringsAsFactors = FALSE
  )
  
  data.set$V14 <- NULL
  
  # First line is nonsense
  headnames <- data.set[2, ] %>%
    as.character
  
  # Subset from the 3rd row onwwards
  data.set <- data.set[3:(data.set %>% nrow), ]
  
  # Append data frame header names
  names(data.set) <- headnames
  
  # Get rid of footnotes
  data.set %<>% subset(
    data.set$City %>%
      strsplit(split = "") %>% 
      purrr::map(1) %>% 
      purrr::flatten_chr() %in% (c(1:9) %>% as.character) %>%
      `!`()
  )
  
  # return data set back
  return(data.set)
}