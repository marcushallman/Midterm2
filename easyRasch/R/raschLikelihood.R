setGeneric("likelihood", function(object="rasch",theta ="numeric"){   
  standardGeneric("likelihood")
})

setMethod("likelihood", "rasch",
          function(object,theta){
            probability(object,theta)   #running this code to make sure the right PQ is in the global environment
            return(prod(pq))                   #prod on pq
            
          }
)
likelihood(new("rasch",name="fff",a=c(1,1,1,1,1,1),y=c(1,0,1,0,1,0)),2)

