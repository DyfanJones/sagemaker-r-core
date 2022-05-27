# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/artifacts.py

#' @include image_uris.R
#' @include jumpstart_constants.R
#' @include jumpstart_enums.R
#' @include jumpstart_accessors.R
#' @include r_utils.R

#' @import R6

# Retrieves the container image URI for JumpStart models.
# Only `model_id`, `model_version`, and `image_scope` are required;
# the rest of the fields are auto-populated.
# Args:
#   model_id (str): JumpStart model ID for which to retrieve image URI.
# model_version (str): Version of the JumpStart model for which to retrieve
# the image URI.
# image_scope (str): The image type, i.e. what it is used for.
# Valid values: "training", "inference", "eia". If ``accelerator_type`` is set,
# ``image_scope`` is ignored.
# framework (str): The name of the framework or algorithm.
# region (str): The AWS region.
# version (str): The framework or algorithm version. This is required if there is
# more than one supported version for the given framework or algorithm.
# py_version (str): The Python version. This is required if there is
# more than one supported Python version for the given framework version.
# instance_type (str): The SageMaker instance type. For supported types, see
# https://aws.amazon.com/sagemaker/pricing/instance-types. This is required if
# there are different images for different processor types.
# accelerator_type (str): Elastic Inference accelerator type. For more, see
# https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html.
# container_version (str): the version of docker image.
# Ideally the value of parameter should be created inside the framework.
# For custom use, see the list of supported container versions:
#   https://github.com/aws/deep-learning-containers/blob/master/available_images.md.
# distribution (dict): A dictionary with information on how to run distributed training
# training_compiler_config (:class:`~sagemaker.training_compiler.TrainingCompilerConfig`):
#   A configuration class for the SageMaker Training Compiler.
# tolerate_vulnerable_model (bool): True if vulnerable versions of model
# specifications should be tolerated (exception not raised). If False, raises an
# exception if the script used by this version of the model has dependencies with known
# security vulnerabilities.
# tolerate_deprecated_model (bool): True if deprecated versions of model
# specifications should be tolerated (exception not raised). If False, raises
# an exception if the version of the model is deprecated.
# Returns:
#   str: the ECR URI for the corresponding SageMaker Docker image.
# Raises:
#   ValueError: If the combination of arguments specified is not supported.
# VulnerableJumpStartModelError: If any of the dependencies required by the script have
# known security vulnerabilities.
# DeprecatedJumpStartModelError: If the version of the model is deprecated.
.retrieve_image_uri = function(model_id,
                               model_version,
                               image_scope,
                               framework=NULL,
                               region=NULL,
                               version=NULL,
                               py_version=NULL,
                               instance_type=NULL,
                               accelerator_type=NULL,
                               container_version=NULL,
                               distribution=NULL,
                               base_framework_version=NULL,
                               training_compiler_config=NULL,
                               tolerate_vulnerable_model,
                               tolerate_deprecated_model){
  if(is.null(region)){
    region = JUMPSTART_DEFAULT_REGION_NAME
  }
  model_specs = verify_model_region_and_return_specs(
    model_id=model_id,
    version=model_version,
    scope=image_scope,
    region=region,
    tolerate_vulnerable_model=tolerate_vulnerable_model,
    tolerate_deprecated_model=tolerate_deprecated_model
  )

  if (image_scope == JumpStartScriptScope$INFERENCE){
    ecr_specs = model_specs$hosting_ecr_specs
  } else if (image_scope == JumpStartScriptScope$TRAINING){
    ecr_specs = model_specs$training_ecr_specs
  }
  if (!is.null(framework) && framework != ecr_specs$framework){
    ValueError$new(
      sprintf("Incorrect container framework '%s' for JumpStart model ID '%s' ", framework, model_id),
      sprintf("and version '%s'.", model_version)
    )
  }
  if (!is.null(version) && version != ecr_specs$framework_version){
    ValueError$new(
      sprintf("Incorrect container framework version '%s' for JumpStart model ID ", version),
      sprintf("'%s' and version '%s'.", model_id, model_version)
    )
  }
  if (!is.null(py_version) && py_version != ecr_specs$py_version){
    ValueError$new(
      sprintf("Incorrect python version '%s' for JumpStart model ID '%s' ", py_version, model_id),
      sprintf("and version '%s'.", model_version)
    )
  }
  base_framework_version_override=NULL
  version_override=NULL
  if (ecr_specs$framework == ModelFramework$HUGGINGFACE){
    base_framework_version_override = ecr_specs$framework_version
    version_override = ecr_specs$huggingface_transformers_version
  }
  if (image_scope == JumpStartScriptScope$TRAINING){
    return(ImageUris$new()$get_training_image_uri(
      region=region,
      framework=ecr_specs$framework,
      framework_version=version_override %||% ecr_specs$framework_version,
      py_version=ecr_specs$py_version,
      image_uri=NULL,
      distribution=NULL,
      compiler_config=NULL,
      tensorflow_version=NULL,
      pytorch_version=base_framework_version_override %||% base_framework_version,
      instance_type=instance_type
    ))
  }
  if (!is.null(base_framework_version_override)){
    base_framework_version_override = sprintf("pytorch%s", base_framework_version_override)
  }
  return(ImageUris$new()$retrieve(
    framework=ecr_specs$framework,
    region=region,
    version=version_override %||% ecr_specs$framework_version,
    py_version=ecr_specs$py_version,
    instance_type=instance_type,
    accelerator_type=accelerator_type,
    image_scope=image_scope,
    container_version=container_version,
    distribution=distribution,
    base_framework_version=base_framework_version_override %||% base_framework_version,
    training_compiler_config=training_compiler_config
  ))
}

# Retrieves the model artifact S3 URI for the model matching the given arguments.
# Args:
#   model_id (str): JumpStart model ID of the JumpStart model for which to retrieve
# the model artifact S3 URI.
# model_version (str): Version of the JumpStart model for which to retrieve the model
# artifact S3 URI.
# model_scope (str): The model type, i.e. what it is used for.
# Valid values: "training" and "inference".
# region (str): Region for which to retrieve model S3 URI.
# tolerate_vulnerable_model (bool): True if vulnerable versions of model
# specifications should be tolerated (exception not raised). If False, raises an
# exception if the script used by this version of the model has dependencies with known
# security vulnerabilities.
# tolerate_deprecated_model (bool): True if deprecated versions of model
# specifications should be tolerated (exception not raised). If False, raises
# an exception if the version of the model is deprecated.
# Returns:
#   str: the model artifact S3 URI for the corresponding model.
# Raises:
#   ValueError: If the combination of arguments specified is not supported.
# VulnerableJumpStartModelError: If any of the dependencies required by the script have
# known security vulnerabilities.
# DeprecatedJumpStartModelError: If the version of the model is deprecated.
.retrieve_model_uri = function(model_id,
                               model_version,
                               model_scope=NULL,
                               region=NULL,
                               tolerate_vulnerable_model,
                               tolerate_deprecated_model){
  if (is.null(region))
    region = JUMPSTART_DEFAULT_REGION_NAME

  model_specs = verify_model_region_and_return_specs(
    model_id=model_id,
    version=model_version,
    scope=model_scope,
    region=region,
    tolerate_vulnerable_model=tolerate_vulnerable_model,
    tolerate_deprecated_model=tolerate_deprecated_model
  )

  if (model_scope == JumpStartScriptScope$INFERENCE) {
    model_artifact_key = model_specs$hosting_artifact_key
  } else if (model_scope == JumpStartScriptScope$TRAINING){
    model_artifact_key = model_specs$training_artifact_key
  }
  bucket = get_jumpstart_content_bucket(region)

  model_s3_uri = sprintf("s3://%s/%s", bucket, model_artifact_key)

  return(model_s3_uri)
}

# Retrieves the script S3 URI associated with the model matching the given arguments.
# Args:
#   model_id (str): JumpStart model ID of the JumpStart model for which to
# retrieve the script S3 URI.
# model_version (str): Version of the JumpStart model for which to
# retrieve the model script S3 URI.
# script_scope (str): The script type, i.e. what it is used for.
# Valid values: "training" and "inference".
# region (str): Region for which to retrieve model script S3 URI.
# tolerate_vulnerable_model (bool): True if vulnerable versions of model
# specifications should be tolerated (exception not raised). If False, raises an
# exception if the script used by this version of the model has dependencies with known
# security vulnerabilities.
# tolerate_deprecated_model (bool): True if deprecated versions of model
# specifications should be tolerated (exception not raised). If False, raises
# an exception if the version of the model is deprecated.
# Returns:
#   str: the model script URI for the corresponding model.
# Raises:
#   ValueError: If the combination of arguments specified is not supported.
# VulnerableJumpStartModelError: If any of the dependencies required by the script have
# known security vulnerabilities.
# DeprecatedJumpStartModelError: If the version of the model is deprecated.
.retrieve_script_uri = function(model_id,
                                model_version,
                                script_scope=NULL,
                                region=NULL,
                                tolerate_vulnerable_model,
                                tolerate_deprecated_model){
  if (is.null(region)){
    region = JUMPSTART_DEFAULT_REGION_NAME
  }
  model_specs = verify_model_region_and_return_specs(
    model_id=model_id,
    version=model_version,
    scope=script_scope,
    region=region,
    tolerate_vulnerable_model=tolerate_vulnerable_model,
    tolerate_deprecated_model=tolerate_deprecated_model
  )

  if (script_scope == JumpStartScriptScope$INFERENCE){
    model_script_key = model_specs$hosting_script_key
  } else if (script_scope == JumpStartScriptScope$TRAINING) {
    model_script_key = model_specs$training_script_key
  }
  bucket = get_jumpstart_content_bucket(region)

  script_s3_uri = sprintf("s3://%s/%s", bucket,  model_script_key)

  return(script_s3_uri)
}

# Retrieves the training hyperparameters for the model matching the given arguments.
# Args:
#   model_id (str): JumpStart model ID of the JumpStart model for which to
# retrieve the default hyperparameters.
# model_version (str): Version of the JumpStart model for which to retrieve the
# default hyperparameters.
# region (str): Region for which to retrieve default hyperparameters.
# include_container_hyperparameters (bool): True if container hyperparameters
# should be returned as well. Container hyperparameters are not used to tune
# the specific algorithm, but rather by SageMaker Training to setup
# the training container environment. For example, there is a container hyperparameter
# that indicates the entrypoint script to use. These hyperparameters may be required
# when creating a training job with boto3, however the ``Estimator`` classes
# should take care of adding container hyperparameters to the job. (Default: False).
# Returns:
#   dict: the hyperparameters to use for the model.
.retrieve_default_hyperparameters = function(model_id,
                                             model_version,
                                             region=NULL,
                                             include_container_hyperparameters=FALSE){
  if (is.null(region)) {
    region = JUMPSTART_DEFAULT_REGION_NAME
  }
  model_specs = JumpStartModelsAccessor$get_model_specs(
    region=region, model_id=model_id, version=model_version
  )

  default_hyperparameters=list()
  for (hyperparameter in model_specs$hyperparameters){
    if ((include_container_hyperparameters && hyperparameter$scope == VariableScope$CONTAINER)
        || hyperparameter$scope == VariableScope$ALGORITHM){
      default_hyperparameters[[hyperparameter$name]] = as.character(hyperparameter$default)
    }
  }
  return(default_hyperparameters)
}

# Retrieves the inference environment variables for the model matching the given arguments.
# Args:
#   model_id (str): JumpStart model ID of the JumpStart model for which to
# retrieve the default environment variables.
# model_version (str): Version of the JumpStart model for which to retrieve the
# default environment variables.
# region (Optional[str]): Region for which to retrieve default environment variables.
# Returns:
#   dict: the inference environment variables to use for the model.
.retrieve_default_environment_variables = function(model_id,
                                                   model_version,
                                                   region){
  if (is.null(region)){
    region = JUMPSTART_DEFAULT_REGION_NAME
  }
  model_specs = JumpStartModelsAccessor$get_model_specs(
    region=region, model_id=model_id, version=model_version
  )

  default_environment_variables=list()
  for (environment_variable in model_specs$inference_environment_variables){
    default_environment_variables[[environment_variable$name]] = as.character(environment_variable$default)
  }
  return(default_environment_variables)
}
