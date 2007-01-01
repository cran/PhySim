"birthdeath.tree" <-
function (b,d,T){
   #December 6 2005 Jason T. Weir (Requires R library APE to be loaded)
   # The following simulates birth death trees to a given time T
   # b = birth rate
   # d = death rate
   # To simulate Yule trees set, d = 0
   # If extinction occurs, extinct lineages are given the label "x", while extant times at time T are numbered
   # Use drop.tip function from R library APE to view tree without extinct lineages.

   #At any point in time in a birth death process, the waiting time to the next
   #speciation event (bi) follows an exponential distribution with mean equal to the inverse of
   #the speciation rate (b) multiplied by the total number of lineages extant in the tree. 
   #Likewise, the waiting time to the next extinction event (di) follows an
   #exponential distribution with mean equal to the inverse of the extinction rate (d)
   #multiplied by the total number of lineages extant in the tree.
   #Starting with a single lineage at t = 0, the next event in the tree
   #can be either a speciation or extinction event. Waiting times to the next speciation and
   #extinction event were drawing randomly from distributions of waiting times as described
   #above. If the waiting time to speciation was shorter than to extinction a speciation event
   #(bifurcation) was added randomly to a lineage in the tree at time = t + t?. If the waiting
   #time to extinction was shortest, then an extinction event was added randomly at time = t + tµ. This


   # matrix with all interal edges of a tree at time t
   edge <- matrix(0, 1, 4) 
   # matrix with all the contemporary lineages of a tree at time t
   current_edge <- matrix(0, 3, 4)    
   current_edge[3,2] = -1 

   #The follow define various variables in simulation process
   edge.length <- numeric(1)
   t <- 0 # time at any point in the tree = t*dt
   n <- 1 #number of extant lineages at any given time point along the tree
   j <- 1 #internal counting for edge table
   node <- 0
   current.lineages <- matrix(0, 1, 1)
   edge.length <- numeric(0)
   tip.label <- character(1)
   tip <- 1
   tipE <- 1
   extinct = 0

   ############
   repeat{#1a
      current_edge4 <- current_edge
      if(n<=0) break
      if(t>=T) break
      #draws a random waiting time (bi) to next birth event 
      bi <- rexp(1,rate=(b*n))
      #draws a random waiting time (di) to next death event
      # sets di to be very large number if d = 0 so that bi < di
      if(d>0) di <- rexp(1,rate=(d*n)) else di = 10000000 

       #adds a branching event to tree if bi < di
        if(bi <= di){  
             if((t + bi) >= T) break
             if((t + bi) <= T) {
                     t <- t + bi


                 #updates the branch lengths and current time for each current_lineage(tip)
                  current_edge[,3] = t
                  current_edge[,4] = current_edge[,4] + bi

                 #randomly chooses one of he current tips in tree as next bifurcation
                  random_lineage <- round(runif(1, min=3, max=(n+2)))
                  
                 #assigns the node number to the randomly chosen tip
                  node <- node - 1
                  current_edge[random_lineage,2] <- node

                 #adds the randomly chosen tip from the current_edge matrix to the edge matrix
                  edge <- rbind(edge, current_edge[random_lineage,])
                 

                 #adds new tips to current_edge and deletes the randomly chosen tip
                  next_edge <- current_edge[random_lineage,2]
                  current_edge2 <- matrix(0, 2, 4)
                  current_edge2[,1] <- next_edge
                  current_edge <- rbind(current_edge, current_edge2)
                  current_edge <- as.matrix(current_edge[-random_lineage,])  # deletes the speciated lineage
                  n <- n + 1
                             }

                 }

       #deletes a lineage from tree (extinction event) if di < bi
        if(di < bi){  
             if((t + di) >= T) break
             if((t + di) <= T) {
                     t <- t + di


                 #updates the branch lengths and current time for each current_lineage(tip)
                  current_edge[,3] = t
                  current_edge[,4] = current_edge[,4] + di

                 #randomly chooses one of he current tips in tree as next bifurcation
                  random_lineage <- round(runif(1, min=3, max=(n+2)))
                  

                 #assigns the node number to the randomly chosen tip
                  extinct <- extinct + 1
                  current_edge[random_lineage,2] <- extinct

                 #adds the randomly chosen tip from the current_edge matrix to the edge matrix
                  edge <- rbind(edge, current_edge[random_lineage,])
                 
                 #deletes the extinct tip from current_edge marix
                  current_edge <- as.matrix(current_edge[-random_lineage,])  

                  n <- n - 1
                              }
               }

   }#1a

   #####################
      # if the waiting time to next event (bi or di) results in a branching event at an age > T, then stops tree without bifurcation
      if(bi <= di) { 
         if((t + bi) >= T) {
                  bii <- T - t
                 #updates the branch lengths and current time for each current_lineage(tip)
                  current_edge[,3] = T
                  current_edge[,4] = current_edge[,4] + bii
                           }
                   }   
       if(di < bi){
          if((t + di) >= T) {
                  dii <- T - t
                 #updates the branch lengths and current time for each current_lineage(tip)
                  current_edge[,3] = T
                  current_edge[,4] = current_edge[,4] + dii
                           }
                   }

   #####################
   #outputs tree in phylo format
   extinct_tips <- extinct
   extant_tips <- n

   if(n>1){
   current_edge[3:(n+2),2] <- (extinct_tips+1): (extant_tips+extinct_tips)
   edge <- rbind(edge, current_edge[3:(n+2),])
   edge <- edge[-2,]
   edge <- edge[-1,]
   tip.label_extinct <- matrix(as.character("x"),1,extinct_tips)
   tip.label_extant <- matrix(as.character((extinct_tips+1): (extant_tips+extinct_tips)),1,extant_tips)
   tip.label <- as.vector(cbind(tip.label_extinct, tip.label_extant))
   }
       edge.length <- edge[,4]
       mode(edge) <- "character"
       obj <- list(edge = edge, edge.length = edge.length, tip.label=tip.label, extinct_tips=extinct_tips, extant_tips=extant_tips, t=t, T=T)
       class(obj) <- "phylo"
       obj

   }

