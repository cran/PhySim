"lagtime" <-
function(phy, rate)
{

###Creates a matrix with all nodes and determines if each is a phylogroup or species
gg <- as.matrix(sort(branchingPhySim.times(phy), decreasing=TRUE))
    tmp <- as.numeric(phy$edge)
    nb.tip <- max(tmp)
    nb.node <- -min(tmp)        
    nb.edge <- nb.tip + nb.node
node_matrix <- matrix(0,nb.node,4)
length2 <- nrow(gg)
#this is necessary becasue I cannot get the name from a matrix with only one element
if(length2 == 1) node_matrix[,1] <- "-1"
if(length2 > 1) node_matrix[,1] <- names(gg[,1])
node_matrix[,2] <- gg[,1]
node_matrix[,3] <- rexp(nb.node, rate=rate)
for(j in 1:nb.node) {
   if(node_matrix[j,2] < node_matrix[j,3]) node_matrix[j,4] <- "P" else node_matrix[j,4] <- "S"
                    }

##################
##################
###In the above matrix it is possible for species to arrise out of a group of nodes that are all phylogroups
###This renders the species paraphyletic and matches how species may form. 
###However, this behavior is not desirable in phylogenetic simulations becasue it is hard to classify branching times of species
###Thus to keep species monophyletic all species level nodes descendent to a phylogroup node must 
###be altered to be phylogroups. Thus once a node is classified as a phylogroup its daughters will always be phylogroups.
##################
##################

delete.phylo.nodes <- function(p_nodes1,phy,node_matrix)
###This is an internal function called only if their are nodes classified as "P" to be deleted
###If determines which nodes need to be deleted and then writes a tree with only nodes labelled "S"
{
repeat{
###Creates a matrix of all "P" nodes with first column containing node names and 
###second containing immediate daughter lineages
 ind <- as.logical(match(phy$edge[, 1], p_nodes1))
 ind[is.na(ind)] <- FALSE
 length2 <- 2 * length(p_nodes1)
 term.br <- matrix(phy$edge[ind], length2, 2) 
 term.br[,2] <- as.numeric(term.br[,2])


###Adds a "P" to every immediate daughter node 
 count <- 0
 length2 <- nrow(term.br)
 for(r in 1:(length2)) {#4aa
       if(term.br[r,2] < 0){#if < 0 it is a daughter node, if > then a terminal tip
            term.br[r,1] <- "P"
            count <- count + 1}
                    }#4bb

 if(count == 0) break #if = 0 it means all immediate daughter nodes are tips, thus break

###Creates a list for those daughter nodes
    ind <- as.logical(match(term.br[, 1], "P"))
    ind[is.na(ind)] <- FALSE
    length2 <- length(ind)
    term.br <- term.br[ind,2]
    phylo_nodes <- term.br             
  
###searches p_nodes1 to see if daughter nodes in phylo_nodes are already present
###If not present then they are added to it
length2 <- length(phylo_nodes)
nodes_added <- 0
for(t in length2:1){#1aa
   length3 <- length(p_nodes1)
   fff <- 0
    for(y in 1:length3){#2aa
         if(p_nodes1[y] == phylo_nodes[t]) fff <- fff + 1
                       }#2bb
   if(fff == 0){#3aa ###if fff=0 it means the node was not already in p_nodes1 and thus is added 
          p_nodes1 <- c(p_nodes1, phylo_nodes[t]) 
          nodes_added <- nodes_added + 1
               }#3bb
                   }#1bb
 if(nodes_added == 0) break
}###This process is now repeated for p_nodes1 until no new nodes are added     


###If all nodes in a tree are rendered as phylogroups then the tree cannot be written 
 length_P_nodes <- length(p_nodes1)
 length_all_nodes <- nrow(node_matrix)
 if(length_P_nodes == length_all_nodes){
     phy <- 0}
 if(length_P_nodes < length_all_nodes){#5aa


###marks the tips for each phylo group node with a "P"

 ind <- as.logical(match(phy$edge[, 1], p_nodes1))
 ind[is.na(ind)] <- FALSE
 length2 <- 2*length(p_nodes1)
 tips <- matrix(phy$edge[ind], length2, 2) 
 tips[,2] <- as.numeric(tips[,2])
 length2 <- nrow(tips)
 for(i in length2:1){
       if(tips[i,2] < 0) tips <- tips[-i,]
                 }
 tips <- tips[,2]
 phy$tip.label[as.numeric(tips)] <- "P"

#save tree here

###creates a list of nodes with internal value "S" called s_nodes1
 all_nodes1 <- node_matrix[,1]
 ind <- as.logical(match(all_nodes1, p_nodes1))
 ind <- as.logical(match(ind, NA))
 ind[is.na(ind)] <- FALSE
 length2 <- length(all_nodes1) - length(p_nodes1)
 s_nodes1 <- (all_nodes1[ind]) 

###creates a matrix identical to nodes_matrix but with all phylogroup nodes excluded
 ind <- as.logical(match(node_matrix[, 1], s_nodes1))
 ind[is.na(ind)] <- FALSE
 length2 <- length(s_nodes1)
 s_node_matrix <- matrix(node_matrix[ind], length2, 4)

###Generates the tree in phylo format for only nodes in s_nodes1
daught <- 1 #this is a counter for terminal tips to provide their names
length2 <- length(s_nodes1)
edge <- matrix(NA,(2*length2),5)
for(q in 1:(length2)){
     s_nodes1_temp <- s_nodes1

    ###the node and daughters of the node
     node <- s_nodes1_temp[q] # the current node
	  
     daughters <- phy$edge[which(phy$edge[, 1] == node), 2]
     daughter1 <- daughters[1]
     daughter2 <- daughters[2]

    ###age of node
     ind <- as.logical(match(s_node_matrix[, 1], node))
     ind[is.na(ind)] <- FALSE
     age.node1 <- matrix(s_node_matrix[ind])
     age.node2 <- age.node1[2,]
     age.node <- as.numeric(age.node2)

    ###age of daughter1
    rrr <- 0
    for(ww in 1:(length2)) if(s_node_matrix[ww,1] == daughter1) rrr <- rrr + 1
         if(rrr == 1){#1aa
            if(as.numeric(daughter1) < 0){#2aa
                ind <- as.logical(match(s_node_matrix[, 1], daughter1))
                ind[is.na(ind)] <- FALSE
                age.daughter1 <- matrix(s_node_matrix[ind]) 
                age.daughter1 <- as.numeric(age.daughter1[2,])   
                                         }#2bb
                     }#1bb
         if(rrr == 0){ 
            age.daughter1 <- 0
            daughter1 <- daught
            daught <- daught + 1}

    ###age of daughter2
    rrr <- 0
    for(ww in 1:(length2)) if(s_node_matrix[ww,1] == daughter2) rrr <- rrr + 1
         if(rrr == 1){#1aa
            if(as.numeric(daughter2) < 0){#2aa
                ind <- as.logical(match(s_node_matrix[, 1], daughter2))
                ind[is.na(ind)] <- FALSE
                age.daughter2 <- matrix(s_node_matrix[ind]) 
                age.daughter2 <- as.numeric(age.daughter2[2,])  
                                          }#2bb                                          
                     }#1bb
         if(rrr == 0){ 
            age.daughter2 <- 0
            daughter2 <- daught
            daught <- daught + 1}

    ###adds node to edge matrix
     pos1 <- (2*q)-1
     pos2 <- 2*q
     edge[pos1:pos2,1] <- node
     edge[pos1,2] <- daughter1
     edge[pos2,2] <- daughter2
     edge[pos1:pos2,3] <- age.node
     edge[pos1,4] <- age.daughter1
     edge[pos2,4] <- age.daughter2
     edge[pos1,5] <- age.node - age.daughter1
     edge[pos2,5] <- age.node - age.daughter2
}

###creates the subsections of an object of class "phylo" and assembles the object
edge.length <- as.numeric(edge[,5])
length2 <- daught-1
tip.label <- as.character(c(1:length2))
 edge <- edge[,-(3:5)]
    mode(edge) <- "character"
    obj <- list(edge = edge, edge.length = edge.length, tip.label=tip.label)
    class(obj) <- "phylo"
    obj
}
}#5bb
#####################This is end of internal function delete.phylo.nodes


###This creates a list of those nodes which are classed as "P" for phylogroup
listP <- match(node_matrix[,4], "P", nomatch=0)
for(i in nb.node:1){
  if(listP[i] == 1) listP[i] <- i else listP <- listP[-i]}
p_nodes1 <- node_matrix[listP,1]
p_nodes <- p_nodes1 #this is the list in which all phylogroup nodes will be stored

###Counts number of nodes in p_nodes that are assigned as phylogroups "P"
###If PP = 0 then there are no phylogroups and tree is saved unaltered
###If PP > 0 then phylo groups are pruned
number_p_nodes <- length(p_nodes)

if(number_p_nodes == 0){
      obj <- phy
                       }

if(number_p_nodes > 0){
         phy <- delete.phylo.nodes(p_nodes1,phy,node_matrix)
         obj <- phy
                       }
obj
}

