\name{birthdeath.tree}
\alias{birthdeath.tree}
\title{Simulate a birth death phylogenetic tree}
\description{
Simulates a birth death tree for a given time period}
\usage{
birthdeath.tree(b, d, T)
}
\arguments{
  \item{b}{birth or speciation rate}
  \item{d}{death or extinction rate}
  \item{T}{number of time units to simulate}
}
\details{
  To simulate pure birth (Yule) trees set extinction to 0. 
}
\value{
  an object of class "phylo". 
  extinct lineages are labelled "x". 
  extant lineages at time T are numbered.
}
\author{Jason T. Weir}
\seealso{\code{\link{birthdeath.simulation}} to simulate a batch of trees}
\examples{
 #to simulate a phylogenetic tree
 tree <- birthdeath.tree(b=0.3, d=0, T=10)
}
\keyword{models}