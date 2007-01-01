"birthdeath.simulation" <-
function (b,d,T,N)
{
#b=birth rate
#d=death rate
#T=Time to simulate
#N = number of trees to simulate

no_lineage <- 0
one_lineage <- 0
extant_lineage <- 0
rr <- 1

file.treeExt <- paste("b_", b, "_d_", d,  "_T",T,"_n",N, ".tree_with_extinct", sep = '')
file.treeNoE <- paste("b_", b, "_d_", d,  "_T",T,"_n",N, ".tree_no_extinct", sep = '')


repeat{
       tree <- birthdeath.tree(b,d,T)
        k <- as.numeric(tree$extant_tips)
       ext <- as.numeric(tree$extinct_tips)
       if(k==1) one_lineage <- one_lineage + 1
       if(k==0) no_lineage <- no_lineage + 1

  if(k>1){##2

                extant_lineage <- extant_lineage + 1
                writePhySim.tree(tree, file = file.treeExt, append = TRUE)

       if(ext>0) { 
                tree$edge <- tree$edge[,-4]
                       tree$edge <- tree$edge[,-3]
                       tree <- dropPhySim.tip(tree, tip="x", trim.internal = TRUE)
                       writePhySim.tree(tree, file = file.treeNoE, append = TRUE)
                 }

       if(ext==0) { 
                   tree$edge <- tree$edge[,-4]
                       writePhySim.tree(tree, file = file.treeNoE, append = TRUE)
                 }               

         }##2
       rr <- rr+1
       if(rr==(N+1)) break
    }
extinct_trees <- as.character(no_lineage)
one_lineage_trees <- as.character(one_lineage)
multiple_lineage_trees <- as.character(extant_lineage)
Results <- list(extinct_trees=extinct_trees, one_lineage_trees=one_lineage_trees, multiple_lineage_trees=multiple_lineage_trees)
    class(Results) <- "Birth death simulation results"
Results
}

