# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/exceptions.py

#' @include jumpstart_constants.R
#' @include error.R

#' @import R6

#' @title Exception raised for bad hyperparameters of a JumpStart model.
#' @export
JumpStartHyperparametersError = R6Class("JumpStartHyperparametersError", inherit = SagemakerError)

#' @title Exception raised when trying to access a JumpStart model specs flagged as vulnerable.
#' @description Raise this exception only if the scope of attributes accessed in the specifications have
#'              vulnerabilities. For example, a model training script may have vulnerabilities, but not
#'              the hosting scripts. In such a case, raise a ``VulnerableJumpStartModelError`` only when
#'              accessing the training specifications.
#' @export
VulnerableJumpStartModelError = R6Class("VulnerableJumpStartModelError",
  inherit = SagemakerError,
  public = list(

    #' @description Instantiates VulnerableJumpStartModelError exception.
    #' @param model_id (Optional[str]): model ID of vulnerable JumpStart model.
    #'              (Default: None).
    #' @param version (Optional[str]): version of vulnerable JumpStart model.
    #'              (Default: None).
    #' @param vulnerabilities (Optional[List[str]]): vulnerabilities associated with
    #'              model. (Default: None).
    #' @param scope (str): JumpStart script scopes
    #' @param message (Optional[str]): error message
    initialize = function(model_id = NULL,
                          version = NULL,
                          vulnerabilities = NULL,
                          scope = NULL,
                          message = NULL){
      if (!is.null(message)){
        self$message
      } else {
        if (any(sapply(list(model_id, version, vulnerabilities, scope), is.null))){
          ValueError$new(
            "Must specify `model_id`, `version`, `vulnerabilities`, and `scope` arguments."
          )
        }
        if (scope == JumpStartScriptScope$INFERENCE){
          self$message = paste(
            sprintf("Version '%s' of JumpStart model '%s'", version, model_id),
            "has at least 1 vulnerable dependency in the inference script.",
            "Please try targetting a higher version of the model.",
            sprintf("List of vulnerabilities: %s", paste(vulnerabilities, collapse = ', '))
          )
        } else if (scope == JumpStartScriptScope$TRAINING){
          self$message = paste(
            sprintf("Version '%s' of JumpStart model '%s'", version, model_id),
            "has at least 1 vulnerable dependency in the training script. ",
            "Please try targetting a higher version of the model. ",
            sprintf("List of vulnerabilities: %s", paste(vulnerabilities, collapse = ', '))
          )
        } else {
          NotImplementedError$new(sprintf(
            "Unsupported scope for VulnerableJumpStartModelError: '%s'",
            scope
          ))
        }
      }
      super$initialize(self$message)
    }
  ),
  lock_objects = F
)

#' @title Exception raised when trying to access a JumpStart model deprecated specifications.
#' @description A deprecated specification for a JumpStart model does not mean the whole model is
#'              deprecated. There may be more recent specifications available for this model. For
#'              example, all specification before version ``2.0.0`` may be deprecated, in such a
#'              case, the SDK would raise this exception only when specifications ``1.*`` are
#'              accessed.
#' @export
DeprecatedJumpStartModelError = R6Class("DeprecatedJumpStartModelError",
  inherit = SagemakerError,
  public = list(

    #' @description Instantiates DeprecatedJumpStartModelError exception.
    #' @param model_id (Optional[str]): model ID of vulnerable JumpStart model.
    #'              (Default: None).
    #' @param version (Optional[str]): version of vulnerable JumpStart model.
    #'              (Default: None).
    #' @param message (Optional[str]): error message
    initialize = function(model_id = NULL,
                          version = NULL,
                          message = NULL){
      if (!is.null(message)) {
        self$message = message
      } else {
        if (any(sapply(list(model_id, version), is.null))){
          ValueError$new("Must specify `model_id` and `version` arguments.")
        }
        self$message = paste(
          sprintf("Version '%s' of JumpStart model '%s' is deprecated.", version, model_id),
          "Please try targetting a higher version of the model."
        )
      }
      super$initialize(self$message)
    }
  ),
  lock_objects = F
)
