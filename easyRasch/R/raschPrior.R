#' Calculates the height of the normal curve evaluated at theta
#'
#' Takes an object of class "rasch" and calculates the height of the normal curve evaluated at theta
#'
#' @param theta A numeric value
#' @return Calculates the height of the normal curve evaluated at theta
#'  \item{output}
#' @author Marcus Hallman: \email{hallman@@wustl.edu}
#' @note This is for a midterm
#' @examples
#' raschPrior(Walt, lower, upper)
#' @seealso \code{\link{raschProbability, raschLikelihood}}
#' @rdname raschPrior
#' @export
setGeneric("prior", function(theta ="numeric"){   
  standardGeneric("prior")
})

setMethod("prior", "rasch",
          function(theta){
            return(dnorm(theta, mean=0, sd=3))      #returns the height of the normal curve evaluated at theta
          }
)
