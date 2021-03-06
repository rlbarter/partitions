% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kd-tree.R
\name{kdTree}
\alias{kdTree}
\title{Build a kd-tree.}
\usage{
kdTree(X, max.depth = 5)
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
Build a kd-tree.
}
\examples{
kdTree(iris[,-5], max.depth = 5)
}

