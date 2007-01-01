\name{lagtime.batch}
\alias{lagtime.batch}
\title{Correct for lag-time to speciation along a series of phylogenetic trees}
\description{
Each node in a series of phylogenetic trees is classified as an interspecific or intraspecific phylogenetic split 
by drawing a lag-time to speciation randomly from a exponential distribution of lag-times. 
Beginning from the root of each tree, once a node is classified as a intraspecific split all of its 
daughter lineages keep the same classification in order to maintain species as monophyletic.
All nodes classified as intras-specific splits are pruned.}
\usage{
lagtime.batch(b, d, n, T, rate)
}
\arguments{
  \item{b}{birth (speciation) rate under which trees are simulated}
  \item{d}{death (extinction) rate under which trees are simulated}
  \item{n}{number of trees in simulation}
  \item{T}{time units trees simulated for}
  \item{rate}{the average lag-time to speciation. 1 / rate is the rate parameter 
   of the exponential distribution used to model speciation along a phylogenetic tree.}
}
\details{
  Reads in a file of trees simulated with parameters b, d, n and T using the command 'birthdeath.simulation'. 
  If b and d are a list, then multiple files will be read.
}
\value{
write a series of phylogenetic trees of class phylo, possessing only inter-specific splits, to a file with '.tree\_no\_extinct' extension.
}
\references{Weir, J. T. and Schluter, D. 2007. Science in press}
\author{Jason T. Weir}
\seealso{\code{\link{lagtime}}, to model a lag-time to speciation along a single phylogenetic tree.}
\keyword{models}
