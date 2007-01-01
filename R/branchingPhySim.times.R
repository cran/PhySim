branchingPhySim.times <- function (phy) 
{
    if (class(phy) != "phylo") 
        stop("object \"phy\" is not of class \"phylo\"")
    tmp <- as.numeric(phy$edge)
    nb.tip <- max(tmp)
    nb.node <- -min(tmp)
    xx <- as.numeric(rep(NA, 1 + nb.node))
    names(xx) <- as.character(c(-(1:nb.node), 1))
    xx["-1"] <- 0
    for (i in 2:length(xx)) {
        nod <- names(xx[i])
        ind <- which(phy$edge[, 2] == nod)
        base <- phy$edge[ind, 1]
        xx[i] <- xx[base] + phy$edge.length[ind]
    }
    depth <- xx[length(xx)]
    branching.times <- depth - xx[-length(xx)]
    if (!is.null(phy$node.label)) 
        names(branching.times) <- phy$node.label
    branching.times
}
