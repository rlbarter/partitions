
#' Build a random-partition tree.
#'
#' @param X A data frame whose columns correspond to the variables and whose
#'          rows correspond to the data points.
#' @param max.depth A number specifying the total depth of the tree.
#' @return A data frame describing the nodes of the tree and which data points
#'          are included in the node.
#' @importFrom magrittr "%>%"
#' @examples
#' rpTree(iris[,-5], max.depth = 5)

rpTree <- function(X, max.depth = 5) {

  # initialize objects
  depth <- 0
  node <- list(left.child = NULL,
               right.child = NULL)
  # add an id column to keep track of which data points go where
  X$id <- 1:nrow(X)

  # results table
  N <- nrow(X)
  # depth is the depth of the tree
  # initialize the results table
  # axis is the random direction selected from  the unit sphere.
  # cutpoint is the point on the random axis where the median lies
  # the remaining variables are binary values describing whether or not 
  #   the data point lies in the partition described by the depth, axis, 
  #   angle and cutpoint.
  results <- data.frame(depth = depth, axis = NA, 
                        cutpoint = NA, 
                        t(rep(TRUE, N)))
  colnames(results) <- c("depth", "axis", "cutpoint", paste0("d", 1:nrow(X)))

  # run the recursive function to define the list of results tables
  nested.list.results <- rpSplit(X, N, node, depth, max.depth)

  clean.results <- nestedListToDataFrame(nested.list.results,
                                         entry.length = N + 3)

  return(clean.results)
}




#' A recursive nodesplitting function called in rpTree()
#'
#' @importFrom magrittr "%>%"

rpSplit <- function(X, N, node, depth, max.depth) {
  if (depth < max.depth) {

    # go one layer deeper
    depth <- depth + 1

    # Obtain a random point on the unit sphere
    U <- runif(ncol(X), -1, 1)
    U <- U / sqrt(sum(U^2))

    # identify the median along the specified axis from the parent node
    projections <- (as.matrix(X) %*% matrix(U)) %*% U
    std.projection <- apply(projection , 1, function(x) x / sqrt(sum(x^2)))
    cutpoint <- 

    ### TODO : idenitfy which points fall on one side of the random hyperplane
    left.of.cutpoint <- (projections %*% matrix(U)) < cutpoint

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
    node$left.child <- rpSplit(X[left.of.cutpoint, ], N,
                               node, depth, max.depth)
    node$right.child <- rpSplit(X[!left.of.cutpoint, ], N,
                                node, depth, max.depth)

    return(node)
  }
}

