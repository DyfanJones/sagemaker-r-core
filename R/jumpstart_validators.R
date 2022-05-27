# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/validators.py

#' @include jumpstart_constants.R
#' @include jumpstart_enums.R
#' @include jumpstart_accessors.R
#' @include jumpstart_exceptions.R
#' @include jumpstart_types.R
#' @include r_utils.R

#' @import R6

# Perform low-level hyperparameter validation on single parameter.
# Args:
#   hyperparameter_name (str): The name of the hyperparameter to validate.
# hyperparameter_value (Any): The value of the hyperparemter to validate.
# hyperparameter_specs (List[JumpStartHyperparameter]): List of ``JumpStartHyperparameter`` to
# use when validating the hyperparameter.
# Raises:
#   JumpStartHyperparametersError: If the hyperparameter is not formatted correctly,
# according to its specs in the model metadata.
.validate_hyperparamter = function(hyperparameter_name,
                                   hyperparameter_value,
                                   hyperparameter_specs){
  hyperparameter_spec = filter(Negate(is.null), lapply(
    hyperparameter_specs, function(spec){
    if(spec$name == hyperparameter_name) spec
  }))

  if (length(hyperparameter_spec) == 0){
    JumpStartHyperparametersError$new(sprintf(
      "Unable to perform validation -- cannot find hyperparameter '%s' ", hyperparameter_name),
      "in model specs."
    )
  }

  if (length(hyperparameter_spec) > 1){
    JumpStartHyperparametersError$new(
      "Unable to perform validation -- found multiple hyperparameter ",
      sprintf("'%s' in model specs.", hyperparameter_name)
    )
  }

  hyperparameter_spec = hyperparameter_spec[[1]]

  if (hyperparameter_spec$type == VariableTypes$BOOL){
    if (is.logical(hyperparameter_value))
      return(NULL)
    if (!is.character(hyperparameter_value))
      JumpStartHyperparametersError$new(sprintf(
        "Expecting boolean valued hyperparameter, but got '%s'.", as.character(hyperparameter_value))
      )
    if (!(tolower(as.character(hyperparameter_value)) %in% c("true", "false"))){
      JumpStartHyperparametersError$new(sprintf(
        "Expecting boolean valued hyperparameter, but got '%s'.", as.character(hyperparameter_value))
      )
    }
  } else if (hyperparameter_spec$type == VariableTypes$TEXT){
    if (!is.character(hyperparameter_value))
      JumpStartHyperparametersError$new(
        "Expecting text valued hyperparameter to have string type."
      )

    if (hasattr(hyperparameter_spec, "options")){
      if (!(hyperparameter_value %in% hyperparameter_spec$options))
          JumpStartHyperparametersError$new(
            sprintf("Hyperparameter '%s' must have one of the following ", hyperparameter_name),
            sprintf("values: %s.", paste(hyperparameter_spec$options, collapse = ", "))
        )
    }
    if (!is.null(hyperparameter_spec$min)){
      if (length(hyperparameter_value) < hyperparameter_spec$min)
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' must have length no less than ", hyperparameter_name),
          sprintf("%s.", hyperparameter_spec$min)
        )
    }
    if (!is.null(hyperparameter_spec$exclusive_min)){
      if (length(hyperparameter_value) <= hyperparameter_spec$exclusive_min)
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' must have length greater than ", hyperparameter_name),
          sprintf("%s.", hyperparameter_spec$exclusive_min)
        )
    }
    if (!is.null(hyperparameter_spec$max)){
      if (length(hyperparameter_value) > hyperparameter_spec$max)
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' must have length no greater than ", hyperparameter_name),
          sprintf("%s.", hyperparameter_spec$max)
        )
    }
    if (!is.null(hyperparameter_spec$exclusive_max)){
      if (length(hyperparameter_value) >= hyperparameter_spec$exclusive_max)
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' must have length less than ", hyperparameter_name),
          sprintf("%s.", hyperparameter_spec$exclusive_max)
        )
    }
  } else if (hyperparameter_spec$type %in% c(VariableTypes$INT, VariableTypes$FLOAT)){
    tryCatch({
      numeric_hyperparam_value = float(hyperparameter_value)
    }, error = function(e){
      JumpStartHyperparametersError$new(
        sprintf("Hyperparameter '%s' must be numeric type ", hyperparameter_name),
        sprintf("('%s').", hyperparameter_value)
      )
    })

    if (hyperparameter_spec$type == VariableTypes$INT){
      hyperparameter_value_str = as.character(hyperparameter_value)
      start_index = 1
      if (substr(hyperparameter_value_str, 0, 1) %in% c("+", "-"))
        start_index = 2
      if (!grepl("^[0-9]{1,}$", substr(hyperparameter_value_str, start_index, nchar(hyperparameter_value_str))))
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' must be integer type ",hyperparameter_name),
          sprintf("('%s').", hyperparameter_value)
        )
    }

    if (!is.null(hyperparameter_spec$min)){
      if (numeric_hyperparam_value < hyperparameter_spec$min)
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' can be no less than ", hyperparameter_name),
          sprintf("%s.", hyperparameter_spec$min)
        )
    }
    if (!is.null(hyperparameter_spec$max)){
      if (numeric_hyperparam_value > hyperparameter_spec$max)
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' can be no greater than ", hyperparameter_name),
          sprintf("%s.", hyperparameter_spec$max)
        )
    }
    if (!is.null(hyperparameter_spec$exclusive_min)){
      if (numeric_hyperparam_value <= hyperparameter_spec$exclusive_min)
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' must be greater than ", hyperparameter_name),
          sprintf("%s.", hyperparameter_spec$exclusive_min)
        )
    }
    if (!is.null(hyperparameter_spec$exclusive_max)){
      if (numeric_hyperparam_value >= hyperparameter_spec$exclusive_max)
        JumpStartHyperparametersError$new(
          sprintf("Hyperparameter '%s' must be less than ", hyperparameter_name),
          sprintf("%s.", hyperparameter_spec$exclusive_max)
        )
    }
  }
}

#' @title Validate hyperparameters for JumpStart models.
#' @param model_id (str): Model ID of the model for which to validate hyperparameters.
#' @param model_version (str): Version of the model for which to validate hyperparameters.
#' @param hyperparameters (dict): Hyperparameters to validate.
#' @param validation_mode (HyperparameterValidationMode): Method of validation to use with
#'              hyperparameters. If set to ``VALIDATE_PROVIDED``, only hyperparameters provided
#'              to this function will be validated, the missing hyperparameters will be ignored.
#'              If set to``VALIDATE_ALGORITHM``, all algorithm hyperparameters will be validated.
#'              If set to ``VALIDATE_ALL``, all hyperparameters for the model will be validated.
#' @param region (str): Region for which to validate hyperparameters. (Default: JumpStart
#'              default region).
#' @export
validate_hyperparameters = function(model_id,
                                    model_version,
                                    hyperparameters,
                                    validation_mode = HyperparameterValidationMode$VALIDATE_PROVIDED,
                                    region = JUMPSTART_DEFAULT_REGION_NAME){
  if (is.null(validation_mode))
    validation_mode = HyperparameterValidationMode$VALIDATE_PROVIDED

  if (is.null(region))
    region = JUMPSTART_DEFAULT_REGION_NAME

  model_specs = JumpStartModelsAccessor$get_model_specs(
    region=region, model_id=model_id, version=model_version
  )
  hyperparameters_specs = model_specs$hyperparameters

  if (validation_mode == HyperparameterValidationMode.VALIDATE_PROVIDED){
    for (hyperparam_name in names(hyperparameters)){
      .validate_hyperparameter(hyperparam_name, hyperparameters[[hyperparam_value]], hyperparameters_specs)
    }
  } else if (validation_mode == HyperparameterValidationMode.VALIDATE_ALGORITHM){
    for (hyperparam in hyperparameters_specs){
      if (hyperparam$scope == VariableScope$ALGORITHM){
        if (!(hyperparam$name %in% hyperparameters)){
          JumpStartHyperparametersError$new(
            sprintf("Cannot find algorithm hyperparameter for '%s'.", hyperparam$name)
          )
        }
        .validate_hyperparameter(
          hyperparam$name, hyperparameters[[hyperparam$name]], hyperparameters_specs
        )
      }
    }
  } else if (validation_mode == HyperparameterValidationMode.VALIDATE_ALL) {
    for (hyperparam in hyperparameters_specs) {
      if (!(hyperparam$name %in% hyperparameters)) {
        JumpStartHyperparametersError$new(
          srprintf("Cannot find hyperparameter for '%s'.", hyperparam$name)
        )
      }
      .validate_hyperparameter(
        hyperparam$name, hyperparameters[[hyperparam$name]], hyperparameters_specs
      )
    }
  } else {
    NotImplementedError$new(
      sprintf("Unable to handle validation for the mode '%s'.", validation_mode)
    )
  }
}
