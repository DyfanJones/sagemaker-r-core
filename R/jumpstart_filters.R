# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/exceptions.py

#' @include jumpstart_types.R
#' @include r_utils.R

#' @import R6

#' @title Enum class for boolean values.
#' @description This is a status value that an ``Operand`` can resolve to.
#' @export
BooleanValues = Enum(
  "TRUE" = "true",
  "FALSE" = "false",
  "UNKNOWN" = "unknown",
  "UNEVALUATED" = "unevaluated",
  .class = "BooleanValues"
)

#' @title Enum class for filter operators for JumpStart models.
#' @export
FilterOperators = Enum(
  "EQUALS" = "equals",
  "NOT_EQUALS" = "not_equals",
  "IN" = "in",
  "NOT_IN" = "not_in",
  .class = "FilterOperators"
)

#' @title Enum class for special supported filter keys.
#' @export
SpecialSupportedFilterKeys = Enum(
  TASK = "task",
  FRAMEWORK = "framework",
  SUPPORTED_MODEL = "supported_model",
  .class = "SpecialSupportedFilterKeys"
)

FILTER_OPERATOR_STRING_MAPPINGS = list(
  "equals"=list("===", "==", "equals", "is"),
  "not_equals"=list("!==", "!=", "not equals", "is not"),
  "in"=list("in"),
  "not_in"=list("not in")
)

.PAD_ALPHABETIC_OPERATOR = function(operator){
  is_alpha = grepl("[a-z]", operator, ignore.case = T)
  operator[is_alpha] = sprintf(" %s ", operator[is_alpha])
  return(operator)
}

ACCEPTABLE_OPERATORS_IN_PARSE_ORDER = c(
  .PAD_ALPHABETIC_OPERATOR(FILTER_OPERATOR_STRING_MAPPINGS[[FilterOperators$NOT_EQUALS]]),
  .PAD_ALPHABETIC_OPERATOR(FILTER_OPERATOR_STRING_MAPPINGS[[FilterOperators$NOT_IN]]),
  .PAD_ALPHABETIC_OPERATOR(FILTER_OPERATOR_STRING_MAPPINGS[[FilterOperators$EQUALS]]),
  .PAD_ALPHABETIC_OPERATOR(FILTER_OPERATOR_STRING_MAPPINGS[[FilterOperators$IN]])
)

SPECIAL_SUPPORTED_FILTER_KEYS = as.list(SpecialSupportedFilterKeys)

#' @title Operand class for filtering JumpStart content.
#' @export
Operand = R6Class("Operand",
  public = list(

    #' @description Initialize Operand Class
    #' @param unresolved_value (Any): The unresolved value of the operator.
    #' @param resolved_value (BooleanValues): The resolved value of the operator.
    initialize = function(unresolved_value,
                          resolved_value = BooleanValues$UNEVALUATED){
      self$unresolved_value = unresolved_value
      private$.resolved_value = match.arg(resolved_value, unname(as.list(BooleanValues)))
    },

    #' @description Validate operand and return ``Operand`` object.
    #' @param operand (Any): The operand to validate.
    validate_operand = function(operand){
      if (is.character(operand)){
        if (tolower(operand) == tolower(BooleanValues$`TRUE`)) {
        operand = Operand$new(operand, resolved_value=BooleanValues$`TRUE`)
        } else if (tolower(operand) == tolower(BooleanValues$`FALSE`)) {
          operand = Operand$new(operand, resolved_value=BooleanValues$`FALSE`)
        } else if (tolower(operand) == tolower(BooleanValues$UNKNOWN)) {
          operand = Operand$new(operand, resolved_value=BooleanValues$UNKNOWN)
        } else {
          operand = Operand$new(parse_filter_string(operand))
        }
      } else if (!inherits(operand, "Operand")) {
        RuntimeError$new(sprintf("Operand '%s' is not supported.", operand))
      }
    },

    #' @description Evaluates operand.
    eval = function(){
      return(NULL)
    },

    #' @description format class
    format = function(){
      format_class(self)
    }
  ),

  active = list(

    #' @field resolved_value
    #' Getter method for resolved_value.
    resolved_value = function(new_resolved_value){
      if (missing(new_resolved_value))
        return(private$.resolved_value)
      private$.resolved_value = match.arg(resolved_value, unname(as.list(BooleanValues)))
    }
  ),
  private = list(
    .resolved_value = NULL
  ),
  lock_objects = F
)

#' @title Operator class for filtering JumpStart content.
#' @description An operator in this case corresponds to an operand that is also an operation.
#'              For example, given the expression ``(True or True) and True``,
#'              ``(True or True)`` is an operand to an ``And`` expression, but is also itself an
#'              operator. ``(True or True) and True`` would also be considered an operator.
#' @export
Operator = R6Class("Operator",
  inherit = Operand,
  public = list(

    #' @description Initializes ``Operator`` instance.
    #' @param resolved_value (BooleanValues): Optional. The resolved value of the operator.
    #'              (Default: BooleanValues.UNEVALUATED).
    #' @param unresolved_value (Any): Optional. The unresolved value of the operator.
    #'              (Default: None).
    initialize = function(resolved_value = BooleanValues$UNEVALUATED,
                          unresolved_value = NULL){
      super$initialize(unresolved_value=unresolved_value, resolved_value=resolved_value)
    },

    #' @description Evaluates operator.
    eval = function(){
      return(NULL)
    }
  ),
  lock_objects = F
)

#' @title And operator class for filtering JumpStart content.
#' @export
And = R6Class("And",
  inherit = Operator,
  public = list(

    #' @description Instantiates And object.
    #' @param ... (Operand): Operand for And-ing.
    initialize = function(...){
      self$operands = list(...)  # type: ignore
      for (i in 1:length(self$operands)){
        self$operands[[i]] = Operand$public_method$validate_operand(self$operands[[i]])
      }
      super$initialize()
    },

    #' @description Evaluates operator.
    eval = function(){
      incomplete_expression = FALSE
      for (operand in self$operands){
        if (inherits(operand, "Operand")){
          RuntimeError$new(
            sprintf("Operand must be subclass of ``Operand``, but got %s", paste(class(operand), collapse=", "))
          )
        }
        if (operand$resolved_value == BooleanValues$UNEVALUATED){
          operand$eval()
        }
        if (operand$resolved_value == BooleanValues$UNEVALUATED){
          RuntimeError$new(
            "Operand remains unevaluated after calling ``eval`` function."
          )
        }
        if (operand$resolved_value == BooleanValues$`FALSE`){
          self$resolved_value = BooleanValues$`FALSE`
          return(NULL)
        }
        if (operand$resolved_value == BooleanValues$UNKNOWN){
          incomplete_expression = TRUE
        }
      }
      if (!incomplete_expression) {
        self$resolved_value = BooleanValues$`TRUE`
      } else {
        self$resolved_value = BooleanValues$UNKNOWN
      }
    }
  ),
  lock_objects = F
)

#' @title Constant operator class for filtering JumpStart content.
#' @export
Constant = R6Class("Constant",
  inherit = Operator,
  public = list(

    #' @description Instantiates Constant operator object.
    #' @param constant (BooleanValues): Value of constant.
    initialize = function(constant){
      super$initialize(constant)
    },

    #' @description Evaluates constant
    eval = function(){
      return(NULL)
    }
  ),
  lock_objects = F
)

#' @title Identity operator class for filtering JumpStart content.
#' @export
Identity = R6Class("Identity",
  inherit = Operator,
  public = list(

    #' @description Instantiates Identity object.
    #' @param operand (Union[Operand, str]): Operand for identity operation.
    initialize = function(operand){
      super$initialize()
      self$operand = Operand$public_method$validate_operand(operand)
    },

    #' @description Evaluates operator.
    eval = function(){
      if (!inherits(self$operand, "Operand")){
        RuntimeError$new(
          sprintf("Operand must be subclass of ``Operand``, but got %s", paste(class(self$operand), collapse = ", "))
        )
      }
      if (self$operand$resolved_value == BooleanValues$UNEVALUATED){
        self$operand$eval()
      }
      if (self$operand$resolved_value == BooleanValues$UNEVALUATED){
        RuntimeError$new("Operand remains unevaluated after calling ``eval`` function.")
      }
      if (!inherits(self$operand$resolved_value, "BooleanValues")){
        RuntimeError$new(self.operand.resolved_value)
      }
      self$resolved_value = self$operand$resolved_value
    }
  ),
  lock_objects = F
)

#' @title Or operator class for filtering JumpStart content.
#' @export
Or = R6Class("Or",
  inherit = Operator,
  public = list(

    #' @description Instantiates Or object.
    #' @param ... (Operand): Operand for Or-ing.
    initialize = function(...){
      self$operands = list(...)  # type: ignore
      for (i in seq_long(self$operands)){
        self$operands[[i]] = Operand$validate_operand(self$operands[[i]])
        super$initialize()
      }
    },

    #' @description Evaluates operator.
    eval = function(){
      incomplete_expression = FALSE
      for (operand in self$operands){
        if (!inherits(operand, "Operand")){
          RuntimeError$new(sprintf(
            "Operand must be subclass of ``Operand``, but got %s", paste(class(operand), collapse = ", ")
          ))
        }
        if (operand$resolved_value == BooleanValues$UNEVALUATED)
          operand$eval()
        if (operand$resolved_value == BooleanValues.UNEVALUATED)
          RuntimeError$new(
            "Operand remains unevaluated after calling ``eval`` function."
          )
        if (operand$resolved_value == BooleanValues$`TRUE`) {
          self$resolved_value = BooleanValues$`TRUE`
          return(NULL)
        }
        if (operand$resolved_value == BooleanValues$UNKNOWN){
          incomplete_expression = TRUE
        }
      }
      if (!incomplete_expression) {
        self$resolved_value = BooleanValues$`FALSE`
      } else {
        self$resolved_value = BooleanValues$UNKNOWN
      }
    }
  ),
  lock_objects = F
)

#' @title Not operator class for filtering JumpStart content.
#' @export
Not = R6Class("Not",
  inherit = Operator,
  public = list(

    #' @description Instantiates Not object.
    #' @param operand (Operand): Operand for Not-ing.
    initialize = function(operand){
      self$operand = Operand$validate_operand(operand)
      super$initialize()
    },

    #' @description Evaluates operator.
    eval = function(){
      if (!inherits(self$operand, "Operand")){
        RuntimeError$new(sprintf(
          "Operand must be subclass of ``Operand``, but got %s", paste(class(self$operand), collapse = ", ")
        ))
      }
      if (self$operand$resolved_value == BooleanValues$UNEVALUATED){
        self$operand$eval()
      }
      if (self$operand$resolved_value == BooleanValues.UNEVALUATED){
        RuntimeError$new("Operand remains unevaluated after calling ``eval`` function.")
      }
      if (self$operand$resolved_value == BooleanValues$`TRUE`){
        self$resolved_value = BooleanValues$`FALSE`
        return(NULL)
      }
      if (self$operand$resolved_value == BooleanValues$`FALSE`) {
        self$resolved_value = BooleanValues$`TRUE`
        return(NULL)
      }
      self$resolved_value = BooleanValues$UNKNOWN
    }
  ),
  lock_objects = F
)

#' @title Data holder class to store model filters.
#' @description For a given filter string "task == ic", the key corresponds to
#'              "task" and the value corresponds to "ic", with the operation being
#'              "==".
#' @export
ModelFilter = R6Class("ModelFilter",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @description Instantiates ``ModelFilter`` object.
    #' @param key (str): The key in metadata for the model filter.
    #' @param value (str): The value of the metadata for the model filter.
    #' @param operator (str): The operator used in the model filter.
    initialize = function(key,
                          value,
                          operator){
      self$key = key
      self$value = value
      self$operator = operator
    }
  ),
  private = list(
    .slots = list("key", "value", "operator")
  ),
  lock_objects = F
)

#' @title Parse filter string and return a serialized ``ModelFilter`` object.
#' @param filter_string (str): The filter string to be serialized to an object.
#' @export
parse_filter_string = function(filter_string){
  for (operator in ACCEPTABLE_OPERATORS_IN_PARSE_ORDER){
    split_filter_string = split_str(filter_string, operator)
    if (length(split_filter_string) == 2){
      return(ModelFilter$new(
        key=trimws(split_filter_string[1]),
        value=trimws(split_filter_string[2]),
        operator=trimws(operator),
      ))
    }
  }
}

#' @title Evaluates model filter with cached model spec value, returns boolean.
#' @param model_filter (ModelFilter): The model filter for evaluation.
#' @param cached_model_value (Any): The value in the model manifest/spec that should be used to
#'              evaluate the filter.
#' @export
evaluate_filter_expression = function(model_filter,
                                      cached_model_value){
  if (model_filter$operator %in% FILTER_OPERATOR_STRING_MAPPINGS[[FilterOperators$EQUALS]]){
    model_filter_value = model_filter$value
    if (is.logical(cached_model_value)){
      cached_model_value = tolower(as.character(cached_model_value))
      model_filter_value = tolower(model_filter$value)
    }
    if (as.character(model_filter_value) == as.character(cached_model_value)){
      return(BooleanValues$`TRUE`)
    }
    return(BooleanValues$`FALSE`)
  }
  if (model_filter$operator %in% FILTER_OPERATOR_STRING_MAPPINGS[[FilterOperators$NOT_EQUALS]]){
    if (is.logical(cached_model_value)){
      cached_model_value = tolower(as.character(cached_model_value))
      model_filter$value = tolower(model_filter$value)
    }
    if (as.character(model_filter$value) == as.character(cached_model_value)){
      return(BooleanValues$`FALSE`)
    }
    return(BooleanValues$`TRUE`)
  }
  if (model_filter$operator %in% FILTER_OPERATOR_STRING_MAPPINGS[[FilterOperators$IN]]){
    py_obj = model_filter$value
    if (!is.character(py_obj)){
      return(BooleanValues$`FALSE`)
    }
    if (grepl(cached_model_value, py_obj)){
      return(BooleanValues$`TRUE`)
    }
    return(BooleanValues$`FALSE`)
  }
  if (model_filter$operator %in% FILTER_OPERATOR_STRING_MAPPINGS[[FilterOperators$NOT_IN]]){
    py_obj = model_filter$value
    if (!is.character(py_obj)){
      return(BooleanValues$`TRUE`)
    }
    if (grepl(cached_model_value, py_obj)){
      return(BooleanValues$`FALSE`)
    }
    return(BooleanValues$`TRUE`)
  }
  RuntimeError$new(sprintf("Bad operator: %s", as.character(model_filter$operator)))
}
