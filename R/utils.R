
#' Convert a nested list from kdTree() to a sensible data frame
#'
#' @param nested.list A nested list created from kdTree()
#' @param entry.length The number of rows of each individual data frame
#     (typically equal to nrow(X) + 3).
#' @importFrom magrittr "%>%"

nestedListToDataFrame <- function(nested.list, entry.length) {


  # convert the nested list into one long vector
  long.df <- unlist(nested.list)
  # split up into a single-layer list based on each node
  tmp.list <- split(long.df, rep(1:(length(long.df) / entry.length), each = entry.length))
  # join the data frames together into a long data frame
  df <- do.call(rbind, tmp.list)
  # fix the column names
  colnames(df) <- c("depth", "axis", "cutpoint", paste0("d", 1:(entry.length - 3)))
  df <- df %>% as.data.frame %>% dplyr::arrange(depth, axis)
  return(df)
}
