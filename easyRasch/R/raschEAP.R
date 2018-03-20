#' Calculates the EAP for theta
#'
#' Takes an object of class "rasch" and calculates its EAP theta 
#'
#' @param object An object of class "rasch." 
#' @param lower A numeric value for the lower bound of integration
#' @param upper A numeric value for the upper bound of integration
#' @return Calculates the expected a posteriori value for theta  
#'  \item{output}
#' @author Marcus Hallman
#' @note This is for midterm
#' @examples
#' 
#' a <- c(1:10)
#' y <- c(1,0,1,1,0,1,0,1,0,1)
#' Disney <- new("rasch", name = "Walt", a = a, y = y)
#' lower <- -6
#' upper <- 6
#' raschEAP(Disney, lower, upper)
#' @seealso \code{\link{raschProbability, raschLikelihood, raschPrior}}
#' @rdname raschEAP
#' @export
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