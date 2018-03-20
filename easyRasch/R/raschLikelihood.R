#' Rasch Likelihood
#'
#' Calculates the likliehood of certain combinations of answers to questions
#'
#' @param object An object of class "rasch." 
#' @param theta A numeric theta value
#' @return Calculates Likelihood of a certain combination of answers 
#'  \item{output}
#' @author Marcus Hallman: \email{hallman@@wustl.edu}
#' @note This is for midterm
#' @examples
#' 
#' a <- c(1:10)
#' y <- c(1,0,1,1,0,1,0,1,0,1)
#' Disney <- new("rasch", name = "Walt", a = a, y = y)
#' theta<- 2
#' raschEAP(Disney, theta)
#' @seealso \code{\link{raschProbability}}
#' @rdname raschLikelihood
#' @export
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

