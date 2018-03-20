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
  if(length(a)!=length(y)){return("question and answer vectors must have same length")}  #checks that the vectors are of same length
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
 pij<-exppart/(1-exppart)   #making Pij
 qij<-1-pij                  #making qij
 pq<- c(1:length(pij))       #cant think of a way to use apply here. this part makes an empty vector
 for (i in 1:length(pij)){
   if (y==1) pq[i]=pij[i]
   else pq[i]=qij[i]
 }
 return(pq)
 return(pij)
}
)
  
  
fakea<-c(1,1,1,1,1,1,1,1) 
fakey<-c(1,1,1,1,1,1,1,1)  
fake<-new("rasch","Walt Whitman",fakea,fakey)  
  
 #running it 
  
  current.code <- as.package("easyRasch")
  load_all(current.code)
  document(current.code)  
  