\name{lagtime}
\alias{lagtime}
\title{Correction for lag-time to speciation along a phylogeny}
\description{
Each node is classified as an interspecific or intraspecific phylogenetic split 
by drawing a lag-time to speciaton randomly from a exponential distribution of lag-times. 
Beginning from the root, once a node is classified as a intraspecific split all of its 
daughter lineages keep the same classification in order to maintain species as monophyletic.
All nodes classified as intra-specific splits are pruned.}
\usage{
lagtime(phy, rate)
}
\arguments{
  \item{phy}{a phylogenetic tree of class phylo}
  \item{rate}{the average lag-time to speciation. 1 / rate is the rate parameter 
   of the exponential distribution used to model speciation along a phylogenetic tree.}
}
\value{
returns a phylogenetic tree of class phylo, possessing only inter-specific splits.
}
\references{Weir, J. T. and Schluter, D. 2007. Science in press}
\author{Jason T. Weir}
\seealso{\code{\link{lagtime.batch}}, to model a lag-time to speciation for a batch of phylogenetic trees}
\examples{
 #Apply an average lag-time of 1
 tree <- birthdeath.tree(b=0.3, d=0, T=10)
 tree2 <- lagtime(phy=tree, rate = 1)
}
\keyword{models}
