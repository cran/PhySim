\name{birthdeath.simulation}
\alias{birthdeath.simulation}
\title{Simulate batches of birth death Trees}
\description{
simulates batches of birth death trees for a given time period.
}
\usage{
birthdeath.simulation(b, d, T, N)
}
\arguments{
  \item{b}{birth or speciation rate}
  \item{d}{death or extinction rate}
  \item{T}{number of time units to simulate each tree for}
  \item{N}{number of trees to simulate}
}
\details{
  To simulate pure birth (Yule) trees set extinction to 0.
}
\value{
Writes trees to two files with extinct species included (.treeExt) or excluded (.treeNoE).
Returns the number of trees that went extinct, left one lineage and two or more lineages at time = T.
}
\references{Weir, J. T. and Schluter, D. 2007. Science in press}
\author{Jason T. Weir}
\seealso{\code{\link{birthdeath.tree}} to simulate a single tree}
\keyword{models}
