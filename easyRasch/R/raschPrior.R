setGeneric("prior", function(theta ="numeric"){   
  standardGeneric("prior")
})

setMethod("prior", "rasch",
          function(theta){
            return(dnorm(theta, mean=0, sd=3))      #returns the height of the normal curve evaluated at theta
          }
)
