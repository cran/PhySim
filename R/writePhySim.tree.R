writePhySim.tree <- function (phy, file = "", append = FALSE, format = "Newick", 
    multi.line = TRUE, digits = 10) 
### This function was renamed from function write.tree written by Emmanuel Paradis for an old version of ape (<= ape 1.8-4) and is no longer in use by that package
### This function is used internally by several functions in package PhySim.

{
    brl <- !is.null(phy$edge.length)
    nodelab <- !is.null(phy$node.label)
    cp <- function(s) STRING <<- paste(STRING, s, sep = "")
    add.internal <- function(i) {
        cp("(")
        br <- which(phy$edge[, 1] == i)
        for (j in br) {
            desc <- phy$edge[j, 2]
            if (desc < 0) 
                add.internal(desc)
            else add.terminal(j)
            if (j != br[length(br)]) 
                cp(",")
        }
        cp(")")
        if (nodelab) 
            cp(phy$node.label[-i])
        if (brl) {
            cp(":")
            cp(formatC(phy$edge.length[which(phy$edge[, 2] == 
                i)], format = "f", digits = digits))
        }
    }
    add.terminal <- function(i) {
        cp(phy$tip.label[phy$edge[i, 2]])
        if (brl) {
            cp(":")
            cp(formatC(phy$edge.length[i], format = "f", digits = digits))
        }
    }
    if (class(phy) != "phylo") 
        stop(paste("object \"", deparse(substitute(phy)), "\" is not of class \"phylo\""), 
            sep = "")
    mode(phy$edge) <- "numeric"
    nb.node <- -min(phy$edge)
    STRING <- "("
    br <- which(phy$edge[, 1] == -1)
    for (j in br) {
        desc <- phy$edge[j, 2]
        if (desc < 0) 
            add.internal(desc)
        else add.terminal(j)
        if (j != br[length(br)]) 
            cp(",")
    }
    if (is.null(phy$root.edge)) {
        cp(")")
        if (nodelab) 
            cp(phy$node.label[1])
        cp(";")
    }
    else {
        cp(")")
        if (nodelab) 
            cp(phy$node.label[1])
        cp(":")
        cp(formatC(phy$root.edge, format = "f", digits = digits))
        cp(";")
    }
    if (nchar(STRING) < 80) 
        multi.line <- FALSE
    if (multi.line) {
        tmp <- unlist(strsplit(STRING, NULL))
        wh <- grep("[,:]", tmp)
        fin <- seq(7, length(wh), by = 7)
        fin <- if (fin[length(fin)] == length(wh)) 
            wh[fin]
        else c(wh[fin], length(tmp))
        debut <- c(1, fin[-length(fin)] + 1)
        STRING <- character(length(fin))
        for (i in 1:length(STRING)) STRING[i] <- paste(tmp[debut[i]:fin[i]], 
            collapse = "")
    }
    if (file == "") 
        return(STRING)
    else cat(STRING, file = file, append = append, sep = "\n")
}