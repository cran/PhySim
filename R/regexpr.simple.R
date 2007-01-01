"regexpr.simple" <-
function(a,x){
   result<-list()
   w<-regexpr(a,x)
   result$from<-w[1]
   if(w[1]>-1){
   result$to<-w[1]+attr(w, "match.length")-1
               }
   return(result)
}

