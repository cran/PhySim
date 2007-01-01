"lagtime.batch" <-
function(b,d,n,T,rate){
    for(i in 1:n){
        file_name = paste("b_", b[i], "_d_", d[i], "_T", T[i], "_n", n, ".tree_no_extinct", sep = '')
        tree_no_extinct_phy = paste("b_", b[i], "_d_", d[i], "_phyrate_", rate,"_T", T[i], "_n", n, ".tree_no_extinct", sep = '')
        phy_batch <- readPhySim.tree(file=file_name)
            for(j in 1:length(phy_batch)){
                   phy <- phy_batch[[j]]
                   new_phy <- lagtime(phy, rate)
                        if (is.element("phylo",class(new_phy))) {
                   writePhySim.tree(new_phy, file=tree_no_extinct_phy, append=TRUE, multi.line = FALSE)
                                                                 }
                                         }        
                 }
}

