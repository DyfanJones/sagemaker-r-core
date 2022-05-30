# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/utils.py

#' @include jumpstart_constants.R
#' @include jumpstart_enums.R
#' @include jumpstart_accessors.R
#' @include jumpstart_exceptions.R
#' @include jumpstart_types.R
#' @include r_utils.R

#' @import R6
#' @importFrom utils packageVersion

#' @title Returns formatted string indicating where JumpStart is launched.
#' @export
get_jumpstart_launched_regions_message = function(){
  if (length(JUMPSTART_REGION_NAME_SET) == 0)
    return ("JumpStart is not available in any region.")
  if (length(JUMPSTART_REGION_NAME_SET) == 1){
    region = JUMPSTART_REGION_NAME_SET[1]
    return(sprintf("JumpStart is available in %s region.", region))
  }
  sorted_regions = sort(as.character(JUMPSTART_REGION_NAME_SET))
  if (length(JUMPSTART_REGION_NAME_SET) == 2)
    return (sprintf("JumpStart is available in %s and {sorted_regions[1]} regions.", sorted_regions[1],))

  return(sprintf("JumpStart is available in %s regions.", paste(sorted_regions, collapse = ", and ")))
}

#' @title Returns regionalized content bucket name for JumpStart.
#' @param region (str): AWS region
#' @export
get_jumpstart_content_bucket = function(region){

  if (nzchar(Sys.getenv(ENV_VARIABLE_JUMPSTART_CONTENT_BUCKET_OVERRIDE))) {
    bucket_override = Sys.getenv(ENV_VARIABLE_JUMPSTART_CONTENT_BUCKET_OVERRIDE)
    LOGGER$info("Using JumpStart bucket override: '%s'", bucket_override)
    return(bucket_override)
  }

  bucket_override = JUMPSTART_REGION_NAME_TO_LAUNCHED_REGION_DICT[[region]]$content_bucket
  if(is.null(bucket_override)){
    formatted_launched_regions_str = get_jumpstart_launched_regions_message()
    ValueError$new(
      sprintf("Unable to get content bucket for JumpStart in %s region. ", region),
      formatted_launched_regions_str
    )
  }
  return(bucket_override)
}

#' @title Returns formatted manifest dictionary from raw manifest.
#' @description Keys are JumpStartVersionedModelId objects, values are
#'              ``JumpStartModelHeader`` objects
#' @param manifest : Placeholder
#' @export
get_formatted_manifest = function(manifest){
  manifest_dict = list()
  for (header in manifest) {
    header_obj = JumpStartModelHeader$new(header)
    manifest_dict[[
      JumpStartVersionedModelId$new(header_obj$model_id, header_obj$version)
    ]] = header_obj
  }
  return(manifest_dict)
}

#' @title Returns sagemaker library version.
#' @description If the sagemaker library version has not been set, this function
#'              calls ``parse_sagemaker_version`` to retrieve the version and set
#'              the constant.
#' @export
get_sagemaker_version = function(){
  if (SageMakerSettings$get_sagemaker_version() == "")
    SageMakerSettings$set_sagemaker_version(parse_sagemaker_version())
  return(SageMakerSettings$get_sagemaker_version())
}

#' @title Returns sagemaker library version. This should only be called once.
#' @description Function reads ``__version__`` variable in ``sagemaker`` module.
#'              In order to maintain compatibility with the ``packaging.version``
#'              library, versions with fewer than 2, or more than 3, periods are rejected.
#'              All versions that cannot be parsed with ``packaging.version`` are also
#'              rejected.
parse_sagemaker_version = function(){
  return(packageVersion("sagemaker"))
}

#' @title Determines if `model_id` and `version` input are for JumpStart.
#' @description This method returns True if both arguments are not None, false if both arguments
#'              are None, and raises an exception if one argument is None but the other isn't.
#' @param model_id (str): Optional. Model ID of the JumpStart model.
#' @param version (str): Optional. Version of the JumpStart model.
#' @export
is_jumpstart_model_input = function(model_id,
                                    version){
  if (!is.null(model_id) || !is.null(version)){
    if (is.null(model_id) || is.null(version))
      ValueError$new(
        "Must specify `model_id` and `model_version` when getting specs for ",
        "JumpStart models."
    )
    return(TRUE)
  }
  return(FALSE)
}

#' @title Returns True if URI corresponds to a JumpStart-hosted model.
#' @param uri (Optional[str]): uri for inference/training job.
#' @export
is_jumpstart_model_uri = function(uri){
  bucket = NA
  if (parse_url(uri)$scheme == "s3")
    bucket = parse_s3_url(uri)$bucket
  return(bucket %in% as.character(JUMPSTART_BUCKET_NAME_SET))
}

#' @title Returns True if ``tag_key`` is in the ``tag_array``.
#' @param tag_key (str): the tag key to check if it's already in the ``tag_array``.
#' @param tag_array (List[Dict[str, str]]): array of tags to check for ``tag_key``.
#' @export
tag_key_in_array = function(tag_key, tag_array){
  for (tag in tag_array){
    if (tag_key == tag[["Key"]])
      return(TRUE)
  }
  return(FALSE)
}

#' @title Return the value of a tag whose key matches the given ``tag_key``.
#' @param tag_key (str): AWS tag for which to search.
#' @param tag_array (List[Dict[str, str]]): List of AWS tags, each formatted as dicts.
#' @export
get_tag_value = function(tag_key, tag_array){
  tag_values = unname(tag_array[tag_array %in% tag_key])
  if (length(tag_values) != 1) {
    KeyError$new(sprintf(
      "Cannot get value of tag for tag key '%s' -- found %s ", tag_key , length(tag_values)),
      "number of matches in the tag list."
    )
  }
  return(tag_values[[1]])
}

#' @title Adds ``tag_key`` to ``curr_tags`` if ``uri`` corresponds to a JumpStart model.
#' @param uri (str): URI which may correspond to a JumpStart model.
#' @param tag_key (enums.JumpStartTag): Custom tag to apply to current tags if the URI
#'              corresponds to a JumpStart model.
#' @param curr_tags (Optional[List]): Current tags associated with ``Estimator`` or ``Model``.
#' @export
add_single_jumpstart_tag = function(uri,
                                    tag_key,
                                    curr_tags){
  if (is_jumpstart_model_uri(uri)){
    if (is.null(curr_tags))
      curr_tags = list()
    if (!tag_key_in_array(tag_key, curr_tags))
      curr_tags = list.append(curr_tags,
        list(
          "Key"=tag_key,
          "Value"=uri
        )
      )
  }
  return(curr_tags)
}

#' @title Return default JumpStart base name if a URI belongs to JumpStart.
#' @description If no URIs belong to JumpStart, return None.
#' @param uris (Optional[str]): URI to test for association with JumpStart.
#' @export
get_jumpstart_base_name_if_jumpstart_model = function(uris){
  for (uri in uris) {
    if (is_jumpstart_model_uri(uri))
      return(JUMPSTART_RESOURCE_BASE_NAME)
  }
  return(NULL)
}

#' @title Add custom tags to JumpStart models, return the updated tags.
#' @description No-op if this is not a JumpStart model related resource.
#' @param tags (Optional[List[Dict[str,str]]): Current tags for JumpStart inference
#'              or training job. (Default: None).
#' @param inference_model_uri (Optional[str]): S3 URI for inference model artifact.
#'              (Default: None).
#' @param inference_script_uri (Optional[str]): S3 URI for inference script tarball.
#'              (Default: None).
#' @param training_model_uri (Optional[str]): S3 URI for training model artifact.
#'              (Default: None).
#' @param training_script_uri (Optional[str]): S3 URI for training script tarball.
#'              (Default: None).
#' @export
add_jumpstart_tags = function(tags=NULL,
                              inference_model_uri=NULL,
                              inference_script_uri=NULL,
                              training_model_uri=NULL,
                              training_script_uri=NULL){
  if (!is.null(inference_model_uri)){
    tags = add_single_jumpstart_tag(
      inference_model_uri, JumpStartTag$INFERENCE_MODEL_URI, tags
    )
  }
  if (!is.null(inference_script_uri)) {
    tags = add_single_jumpstart_tag(
      inference_script_uri, JumpStartTag$INFERENCE_SCRIPT_URI, tags
    )
  }
  if (!is.null(training_model_uri)){
    tags = add_single_jumpstart_tag(
      training_model_uri, JumpStartTag$TRAINING_MODEL_URI, tags
    )
  }
  if (!is.null(training_script_uri)){
    tags = add_single_jumpstart_tag(
      training_script_uri, JumpStartTag$TRAINING_SCRIPT_URI, tags
    )
  }
  return(tags)
}

#' @title Updates the tags for the ``sagemaker.model.Model.deploy`` command with any JumpStart tags.
#' @param inference_tags (Optional[List[Dict[str, str]]]): Custom tags to appy to inference job.
#' @param training_tags (Optional[List[Dict[str, str]]]): Tags from training job.
#' @export
update_inference_tags_with_jumpstart_training_tags = function(inference_tags,
                                                              training_tags){
  if (!missing(training_tags)){
    for (tag_key in as.list(JumpStartTag)){
      if (tag_key_in_array(tag_key, training_tags)) {
        tag_value = get_tag_value(tag_key, training_tags)
        if (!is.null(inference_tags)) {
          inference_tags = list()
          if (!tag_key_in_array(tag_key, inference_tags)) {
            inference_tags = list.append(
              inference_tags, list("Key"=tag_key, "Value"=tag_value)
            )
          }
        }
      }
    }
  }
  return(inference_tags)
}

#' @title Verifies that an acceptable model_id, version, scope, and region combination is provided.
#' @param model_id (Optional[str]): model ID of the JumpStart model to verify and
#'              obtains specs.
#' @param version (Optional[str]): version of the JumpStart model to verify and
#'              obtains specs.
#' @param scope (Optional[str]): scope of the JumpStart model to verify.
#' @param region (Optional[str]): region of the JumpStart model to verify and
#'              obtains specs.
#' @param tolerate_vulnerable_model (bool): True if vulnerable versions of model
#'              specifications should be tolerated (exception not raised). If False, raises an
#'              exception if the script used by this version of the model has dependencies with known
#'              security vulnerabilities. (Default: False).
#' @param tolerate_deprecated_model (bool): True if deprecated models should be tolerated
#'              (exception not raised). False if these models should raise an exception.
#'              (Default: False).
verify_model_region_and_return_specs = function(model_id,
                                                version,
                                                scope,
                                                region,
                                                tolerate_vulnerable_model = FALSE,
                                                tolerate_deprecated_model = FALSE){
  if (is.null(scope))
    ValueError$new(
      "Must specify `model_scope` argument to retrieve model ",
      "artifact uri for JumpStart models."
    )

  if (!(scope %in% SUPPORTED_JUMPSTART_SCOPES))
    NotImplementedError$new(
      "JumpStart models only support scopes: ",
      paste(
        as.list(SUPPORTED_JUMPSTART_SCOPES),
        sep = ", ", collapse = ", "
      )
    )

  model_specs = JumpStartModelsAccessor$get_model_specs(
    region=region, model_id=model_id, version=version
  )

  if (
    scope == JumpStartScriptScope$TRAINING
    && !model_specs$training_supported
  ) {
    ValueError$new(sprintf(
      "JumpStart model ID '%s' and version '%s' does not support training.",
      model_id, version)
    )
  }
  if (model_specs$deprecated){
    if (!tolerate_deprecated_model)
      DeprecatedJumpStartModelError$new(model_id=model_id, version=version)
    LOGGER$warn("Using deprecated JumpStart model '%s' and version '%s'.", model_id, version)
  }
  if (scope == JumpStartScriptScope$INFERENCE && model_specs$inference_vulnerable){
    if (!tolerate_vulnerable_model)
      VulnerableJumpStartModelError$new(
        model_id=model_id,
        version=version,
        vulnerabilities=model_specs$inference_vulnerabilities,
        scope=JumpStartScriptScope$INFERENCE
      )
    LOGGER$warn(
      "Using vulnerable JumpStart model '%s' and version '%s' (inference).", model_id, version
    )
  }
  if (scope == JumpStartScriptScope$TRAINING && model_specs$training_vulnerable){
    if (!tolerate_vulnerable_model)
      VulnerableJumpStartModelError$new(
        model_id=model_id,
        version=version,
        vulnerabilities=model_specs$training_vulnerabilities,
        scope=JumpStartScriptScope$TRAINING
      )
    LOGGER$warn(
      "Using vulnerable JumpStart model '%s' and version '%s' (training).", model_id, version
    )
  }
  return(model_specs)
}
