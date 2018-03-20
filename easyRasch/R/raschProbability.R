#' Rasch probability
#'
#' Calculates the probability of certain combinations of answers to questions
#'
#' @param object An object of class "rasch." 
#' @param theta A numeric theta value
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