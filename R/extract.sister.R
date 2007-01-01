"extract.sister" <-
function(phy, ages=TRUE){
   if (class(phy) != "phylo") 
        stop("object \"phy\" is not of class \"phylo\"")
   x<-writePhySim.tree(phy)
   # Test whether there are blanks in "x", in which case remove them
   w<-regexpr(" ",x)
   if(w>-1){
   while(w>-1){
   if(w<nchar(x)){
   x<-paste(substring(x,1,w-1),substring(x,w+1,nchar(x)),sep="")
   }
   else{
   x<-substring(x,1,w-1)
   }
   w<-regexpr(" ",x)
   }
   }

   result<-list()
   result$sisters<-vector()
   i<-1
   w<-regexpr.simple("[(][A-Za-z0-9.,:]+[)]",x)# finds the first sister species pair between brackets
   while(w$from>-1){
   sister<-substring(x, w$from, w$to) # extracts first sister taxon in "x"
   result$sisters[i]<-sister
   i<-i+1
   x<-substring(x,w$to+1,nchar(x)) # cuts sister pair out of "x"
   w<-regexpr.simple("[(][A-Za-z0-9.,:]+[)]",x)# finds the next sister species pair
   }
   if(ages){
   result$sisterage<-unlist(lapply(result$sisters,function(x){
   w<-regexpr.simple("[:][0-9.]+[,]",x)
   as.numeric(substring(x, w$from+1, w$to-1))
   }))
   }
   return(result)
}

