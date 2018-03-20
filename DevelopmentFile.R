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
setGeneric("probability", function(object="rasch"){   
  standardGeneric("probability")
})

setMethod("probability", "rasch",
function(raschObj,theta){
 pn<-length(object@a)
  print(pn)
}
)
  
  
  
  
  
  
 #running it 
  
  current.code <- as.package("easyRasch")
  load_all(current.code)
  document(current.code)  