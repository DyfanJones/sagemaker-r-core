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
      format_class()
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
  )
)

#' @title And operator class for filtering JumpStart content.
#' @export
And = R6Class("And",
  inherit = Operator,
  pubilc = list(

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
      for (operand in self.operands){
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




