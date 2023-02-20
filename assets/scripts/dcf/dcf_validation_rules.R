#draft model to define validation rules 

#abstract_vrule
abstract_vrule <- R6Class("abstract_vrule",
  public = list(
    initialize = function(...){
      
    },
    
    validate = function(value){
      stop("Method not implemented")
    }
    
    
  )
)

#simple_vrule
simple_vrule <- R6Class("simple_vrule",
  inherit = abstract_vrule,
  public = list(
    initialize = function(...){
      
    },
    
    validate = function(value){
      stop("Method not implemented")
    }
    
  )                                  
)

#datatype_vrule
datatype_vrule <- R6Class("datatype_vrule",
  inherit = simple_vrule,
  public = list(
    category = "Data types",
    name = "Data type",
    type = NA,
    initialize = function(type, ...){
      self$type = type
    },
    
    validate = function(value){
      report <-data.frame(
        category = character(0),
        rule = character(0),
        type = character(0),
        message = character(0)
      )
      val = as(value, self$type)
      if(is.na(val)){
        report <- rbind(report,data.frame(
          category = self$category,
          rule = self$name,
          type = "ERROR",
          message = sprintf("Value %s is not %s", value, self$type)
        ))
      }else{
        if(self$type != "logical") if(value != val){
          report <- rbind(report,data.frame(
            category = self$category,
            rule = self$name,
            type = "ERROR",
            message = sprintf("Source value %s is not equal to value (%s) after coercing to type '%s'", value, val, self$type)
          ))
        }
      }
      
      return(report)
    }
  )                                  
)

numeric_vrule <- R6Class("numeric_vrule",
   inherit = datatype_vrule,
   public = list(
     initialize = function(...){
       self$type = "numeric"
       self$name = "Numeric data type"
     }
   )
)

integer_vrule <- R6Class("integer_vrule",
   inherit = datatype_vrule,
   public = list(
     initialize = function(...){
       self$type = "integer"
       self$name = "Integer data type"
     }
   )
)

double_vrule <- R6Class("double_vrule",
   inherit = datatype_vrule,
   public = list(
     initialize = function(...){
       self$type = "double"
       self$name = "Double data type"
     }
   )
)

logical_vrule <- R6Class("logical_vrule",
   inherit = datatype_vrule,
   public = list(
     initialize = function(...){
       self$type = "logical"
       self$name = "Logical data type"
     }
   )
)


#complex_vrule
complex_vrule <- R6Class("complex_vrule",
  inherit = abstract_vrule, 
  public = list(
    initialize = function(...){
      
    },
    
    validate = function(value){
      stop("Method not implemented")
    }
  )
)

#op_vrule
op_vrule <- R6Class("op_vrule",
  inherit = abstract_vrule,
  public = list(
    category = "Operators",
    operator = NA,
    rules = list(),
    initialize = function(operator, ...){
      self$operator = operator
      rules = list(...)
      if(!all(sapply(rules, is, "abstract_vrule"))){
        stop("At least one rule defined is not an object extending 'abstract_vrule'")
      }
      self$rules = rules
    },
    
    validate = function(value){
      report <- do.call("rbind", lapply(self$rules, function(rule){
        rule$validate(value)
      }))
      return(report)
    }
  )
)

#and_vrule
and_vrule <- R6Class("and_vrule",
  inherit = op_vrule,
  public = list(
    initialize = function(...){
      super$initialize(operator = "&", ...)
    },
    
    validate = function(value){
      report <- super$validate(value)
      if(nrow(report)>0) if(any(report$type == "ERROR")){
        report <- rbind(report, data.frame(
          category = self$category,
          rule = "And operator",
          type = "ERROR",
          message = sprintf("Value %s is not valid because at least one validation rule is not fulfilled", value)
        ))
      }
      return(report)
    }
  )
)

#rowbased_vrule
rowbased_vrule <- R6Class("rowbased_vrule",
   inherit = complex_vrule,
   public = list(
     initialize = function(...){
       
     },
     
     validate = function(value){
       stop("Method not implemented")
     }
   )
)

#colbased_vrule
colbased_vrule <- R6Class("colbased_vrule",
  inherit = complex_vrule,
  public = list(
    initialize = function(...){
      
    },
    
    validate = function(value){
      stop("Method not implemented")
    }
  )
)

