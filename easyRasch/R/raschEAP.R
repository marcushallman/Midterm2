setGeneric("EAP", function(object="rasch",lower ="numeric", upper = "numeric"){   
  standardGeneric("EAP")
})

setMethod("EAP", "rasch",
          function(object, lower=-6, upper=6){       #set the defaults on lower and upper
            toppart<-integrate(function(i){i*likelihood(object,i)*prior(i)},lower,upper)        #I'm pretty sure this integrates over theta by replacing all the thetas in all my other functions with the integrands
            bottompart<-integrate(function(i){likelihood(object,i)*prior(i)},lower,upper)
            return(toppart/bottompart)          #this gives us the EAP value of theta by dividing the top part by the bottom part
          }
)