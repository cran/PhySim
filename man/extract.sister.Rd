\name{extract.sister}
\alias{extract.sister}
\title{Extract sister species pairs}
\description{
Extracts node ages for sister species pairs (cherries) from phylogenetic trees
}
\usage{
extract.sister(phy, ages = TRUE)
}
\arguments{
  \item{phy}{an object of class phylo}
  \item{ages}{an undeveloped option which should not be altered}
}
\value{
  returns a list of sister species ages.
}
\references{Weir, J. T. and Schluter, D. 2007. Science in press}
\author{Dolph Schluter}

\examples{
 tree <- birthdeath.tree(b=0.3, d=0, T=10)
 extract.sister(tree, ages=TRUE)
}
\keyword{univar}

