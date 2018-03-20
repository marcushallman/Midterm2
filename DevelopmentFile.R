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
 qij<<-1-pij                  #making qij
 pq<<- c(1:length(pij))       #cant think of a way to use apply here. this part makes an empty vector
 for (i in 1:length(pij)){
   if (object@y[i]==1) pq[i]=pij[i]
   else pq[i]=qij[i]                   #this part is and creating the pq object in the global environment
 }
 print(pq)
 print(pij)
}
)
  
fakea<-c(1,1,1,1,1,1,1,1) 
fakey<-c(1,1,1,1,1,1,1,1)  
blank<-new("rasch", name="Walt Whitman", a=c(1,1,1), y=c(1,0,1))
probability(blank,2)
  
 #running it 
  
  current.code <- as.package("easyRasch")
  load_all(current.code)
  document(current.code)  
  