
#' Build a kd-tree.
#'
#' @param X A data frame whose columns correspond to the variables and whose
#'          rows correspond to the data points.
#' @param max.depth A number specifying the total depth of the tree.
#' @return A data frame describing the nodes of the tree and which data points
#'          are included in the node.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' kdTree(iris[,-5], max.depth = 5)

kdTree <- function(X, max.depth = 5) {

  # initialize objects
  depth <- 0
  node <- list(left.child = NULL,
               right.child = NULL)
  # add an id column to keep track of which data points go where
  X$id <- 1:nrow(X)

  # results table
  N <- nrow(X)
  # initialize the results table
  results <- data.frame(depth = depth, axis = NA, cutpoint = NA, t(rep(TRUE, N)))
  colnames(results) <- c("depth", "axis", "cutpoint", paste0("d", 1:nrow(X)))

  # run the recursive function to define the list of results tables
  nested.list.results <- kdSplit(X, N, node, depth, max.depth)

  clean.results <- nestedListToDataFrame(nested.list.results,
                                         entry.length = N + 3)

  return(clean.results)
}




#' A recursive nodesplitting function called in kdTree()
#'
#' @importFrom magrittr "%>%"

kdSplit <- function(X, N, node, depth, max.depth) {
  if (depth < max.depth) {

    # go one layer deeper
    depth <- depth + 1

    # set the axis (ignoring the added id variable)
    axis <- ((depth - 1) %% (ncol(X) - 1)) + 1

    # identify the median along the specified axis from the parent node
    cutpoint <- median(X[, axis])
    # identify the data points that fall to the left of the median cutpoint
    left.of.cutpoint <- X[, axis] < cutpoint

    # tmp keeps track of which data points are present in this node
    # it is a vector of TRUE/FALSE
    tmp <- t(data.frame(1:N %in% X$id))
    rownames(tmp) <- NULL
    colnames(tmp) <- paste0("d", 1:N)
    # fill in the results table for this node
    node$results <- data.frame(depth = depth,
                               axis = axis,
                               cutpoint = cutpoint,
                               tmp)

    # recursively call the function
    node$left.child <- kdSplit(X[left.of.cutpoint, ], N,
                               node, depth, max.depth)
    node$right.child <- kdSplit(X[!left.of.cutpoint, ], N,
                                node, depth, max.depth)

    return(node)
  }
}

