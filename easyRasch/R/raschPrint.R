setMethod("print", "rasch", 
          function(object){
            print(object@name)
            print(EAP(object))
          }
)
