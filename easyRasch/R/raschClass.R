#' Creates a new rasch class object
#' 
#' Initiallizes rasch class objeccts
#'
#' 
#' An object of the class "rasch" has the following slots:
#' \itemize{
#' \item \code{name} A character string indicating the name of the student
#' \item \code{a} A vector of difficulty values for each test question
#' \item \code{y} A vector of binary values indicating whether the student got the question right (1) or wrong)
#' }
#'
#' @author Marcus Hallman: \email{hallman@@wustl.edu}
#' @rdname raschClass
#' @export
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
