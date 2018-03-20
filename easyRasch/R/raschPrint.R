#' Rasch class print
#'
#' Takes an object of class "rasch" and prints its name and EAP
#'
#' @param object An object of class "rasch." 
#' @return prints the name and EAP  
#'  \item{output}
#' @author Marcus Hallman
#' @note This is for a midterm
#' @seealso \code{\link{raschProbability, raschLikelihood, raschPrior,raschEAP}}
#' @rdname raschPrint
#' @export
setMethod("print", "rasch", 
          function(object){
            print(object@name)
            print(EAP(object))
          }
)
