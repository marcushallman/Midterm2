## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("C:/Users/Marcus/Desktop/midterm") 

#Make a class

setClass(Class="rasch",
         representation = representation(
           name="character",
           a="numeric",
           y="numeric"
         ),
         prototype = prototype(
           name = "",
           a=c(),
           y=c()
         )
)
# A validity checker
setValidity("rasch", function(object){
  if(length(object@a)!=length(object@y)){return("question and answer vectors must have same length")}  #checks that the vectors are of same length
})


# A constructor
setMethod("initialize", "rasch", function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
})



#Probability

# a generic Probability function
setGeneric("probability", function(object="rasch",theta ="numeric"){   
  standardGeneric("probability")
})

setMethod("probability", "rasch",
function(object,theta){
  exppart<-theta-sapply(object@a,exp)
 pij<<-exppart/(1-exppart)   #making Pij and creating the obhect in the global environment
 qij<<-1-pij                  #making qij and creating the object in the global environment
 pq<- c(1:length(pij))       #cant think of a way to use apply here. this part makes an empty vector
 for (i in 1:length(pij)){
   if (object@y[i]==1) pq[i]=pij[i]
   else pq[i]=qij[i]                   #this part is populating the pq object
 }
 pq<<-pq   #this part creates the pq object in the global environment, because i can't think of a way to return both pij and pq, and the problem merely asks to 'create the outputs'
 print(pq)
 print(pij)
}
)
  

#Likelihood

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


#Prior
setGeneric("prior", function(theta ="numeric"){   
  standardGeneric("prior")
})

setMethod("prior", "rasch",
function(theta){
        return(dnorm(theta, mean=0, sd=3))      #returns the height of the normal curve evaluated at theta
}
)


#EAP
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

setMethod("print","rasch",
          function(object){
            print(object@name)
            EAP(object)
          }
)

 #running it 
  
  current.code <- as.package("easyRasch")
  load_all(current.code)
  document(current.code)  
  