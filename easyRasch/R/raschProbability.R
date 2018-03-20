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