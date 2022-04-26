# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/enums.py

#' @include r_utils.R

#' @title Enum class for JumpStart model framework.
#' @description The ML framework as referenced in the prefix of the model ID.
#'              This value does not necessarily correspond to the container name.
#' @export
ModelFramework = Enum(
  .class ="ModelFramework",
  PYTORCH = "pytorch",
  TENSORFLOW = "tensorflow",
  MXNET = "mxnet",
  HUGGINGFACE = "huggingface",
  LIGHTGBM = "lightgbm",
  CATBOOST = "catboost",
  XGBOOST = "xgboost",
  SKLEARN = "sklearn"
)

#' @title Possible value of the ``scope`` attribute for a hyperparameter or environment variable.
#' @description Used for hosting environment variables and training hyperparameters.
#' @export
VariableScope = Enum(
  .class ="VariableScope",
  CONTAINER = "container",
  ALGORITHM = "algorithm"
)

#' @title Enum class for JumpStart script scopes.
#' @export
JumpStartScriptScope = Enum(
  .class = "JumpStartScriptScope",
  INFERENCE = "inference",
  TRAINING = "training"
)

#' @title Possible modes for validating hyperparameters.
#' @export
HyperparameterValidationMode = Enum(
  .class = "HyperparameterValidationMode",
  VALIDATE_PROVIDED = "validate_provided",
  VALIDATE_ALGORITHM = "validate_algorithm",
  VALIDATE_ALL = "validate_all"
)

#' @title Possible types for hyperparameters and environment variables.
#' @export
VariableTypes = Enum(
  .class = "VariableTypes",
  TEXT = "text",
  INT = "int",
  FLOAT = "float",
  BOOL = "bool"
)

#' @title Enum class for tag keys to apply to JumpStart models.
#' @export
JumpStartTag = Enum(
  .class = "JumpStartTag",
  INFERENCE_MODEL_URI = "aws-jumpstart-inference-model-uri",
  INFERENCE_SCRIPT_URI = "aws-jumpstart-inference-script-uri",
  TRAINING_MODEL_URI = "aws-jumpstart-training-model-uri",
  TRAINING_SCRIPT_URI = "aws-jumpstart-training-script-uri"
)
