% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rp-tree.R
\name{rpTree}
\alias{rpTree}
\title{Build a random-partition tree.}
\usage{
rpTree(X, max.depth = 5)
}
\arguments{
\item{X}{A data frame whose columns correspond to the variables and whose
rows correspond to the data points.}

\item{max.depth}{A number specifying the total depth of the tree.}
}
\value{
A data frame describing the nodes of the tree and which data points
         are included in the node.
}
\description{
Build a random-partition tree.
}
\examples{
rpTree(iris[,-5], max.depth = 5)
}

