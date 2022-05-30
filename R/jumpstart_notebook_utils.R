# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/notebook_utils.py

#' @include jumpstart_accessors.R
#' @include jumpstart_constants.R
#' @include jumpstart_enums.R
#' @include jumpstart_filters.R
#' @include jumpstart_utils.R
#' @include r_utils.R

#' @import R6
#' @import listenv

# Performs comparison of sdk specs paths, in order to sort them.
# Args:
# model_version_1 (Tuple[str, str]): The first model ID and version tuple to compare.
# model_version_2 (Tuple[str, str]): The second model ID and version tuple to compare.
.compare_model_version_tuples = function(model_version_1 = NULL,
                                         model_version_2 = NULL){
  if (is.null(model_version_1) || is.null(model_version_2)){
    if (!is.null(model_version_2))
      return(-1)
    if (!is.null(model_version_1))
      return(1)
    return(0)
  }
  model_id_1 = model_version_1[[1]]
  version_1 = model_version_1[[2]]

  model_id_2 = model_version_2[[1]]
  version_2 = model_version_2[[2]]

  if (model_id_1 < model_id_2)
    return(-1)

  if (model_id_2 < model_id_1)
    return(1)

  if (numeric_version(version_1) < numeric_version(version_2))
    return(1)

  if (numeric_version(version_2) < numeric_version(version_1))
    return(-1)

  return(0)
}

# Generator for model filters in an operator.
.model_filter_in_operator_generator = function(filter_operator){
  return(
    Filter(Negate(is.null),
      lapply(as.list(filter_operator), function(operator) {
        inherits(operator$unresolved_value, "ModelFilter")
      })
  ))
}

# Iterate over the operators in the filter, assign resolved value if found in second arg.
# If not found, assigns ``UNKNOWN``.
.put_resolved_booleans_into_filter = function(filter_operator,
                                              model_filters_to_resolved_values){
  for (operator in .model_filter_in_operator_generator(filter_operator)){
    model_filter = operator$unresolved_value
    operator$resolved_value = model_filters_to_resolved_values[[
      model_filter]] %||% BooleanValues$UNKNOWN
  }
}

# Iterate over the model filters, if the filter key has a cached value, evaluate the filter.
# The resolved filter values are placed in ``model_filters_to_resolved_values``.
.populate_model_filters_to_resolved_values = function(manifest_specs_cached_values,
                                                      model_filters_to_resolved_values,
                                                      model_filters){
  for (model_filter in model_filters){
    if (names(model_filter) %in% manifest_specs_cached_values) {
      cached_model_value = manifest_specs_cached_values[[model_filter$key]]
      evaluated_expression = evaluate_filter_expression(
        model_filter, cached_model_value
      )
      model_filters_to_resolved_values[[model_filter]] = evaluated_expression
    }
  }
}

#' @title Parse the model ID, return a tuple framework, task, rest-of-id.
#' @param model_id (str): The model ID for which to extract the framework/task/model.
extract_framework_task_model = function(model_id){
  .id_parts = split_str(model_id, "-")

  if (length(.id_parts) < 3){
    ValueError$new(sprintf("incorrect model ID: %s.", model_id))
  }

  framework = .id_parts[1]
  task = .id_parts[2]
  name = paste(.id_parts[3:length(.id_parts)], collapse = "-")

  return(list(framework, task, name))
}

#' @title List tasks for JumpStart, and optionally apply filters to result.
#' @param filter (Union[Operator, str]): Optional. The filter to apply to list tasks. This can be
#'              either an ``Operator`` type filter (e.g. ``And("task == ic", "framework == pytorch")``),
#'              or simply a string filter which will get serialized into an Identity filter.
#'              (e.g. ``"task == ic"``). If this argument is not supplied, all tasks will be listed.
#'              (Default: Constant(BooleanValues$`TRUE`)).
#' @param region (str): Optional. The AWS region from which to retrieve JumpStart metadata regarding
#'              models. (Default: JUMPSTART_DEFAULT_REGION_NAME()).
#' @export
list_jumpstart_tasks = function(filter = Constant$new(BooleanValues$`TRUE`),
                                region = JUMPSTART_DEFAULT_REGION_NAME()){
  tasks = lapply(
    .generate_jumpstart_model_versions(filter=filter, region=region),
    function(ll){
      fw_ll = extract_framework_task_model(ll$model_id)
      return(fw_ll[[2]])
  })
  return(tasks)
}

#' @title List frameworks for JumpStart, and optionally apply filters to result.
#' @param filter (Union[Operator, str]): Optional. The filter to apply to list frameworks. This can be
#'              either an ``Operator`` type filter (e.g. ``And("task == ic", "framework == pytorch")``),
#'              or simply a string filter which will get serialized into an Identity filter.
#'              (eg. ``"task == ic"``). If this argument is not supplied, all frameworks will be listed.
#'              (Default: Constant(BooleanValues$TRUE)).
#' @param region (str): Optional. The AWS region from which to retrieve JumpStart metadata regarding
#'              models. (Default: JUMPSTART_DEFAULT_REGION_NAME()).
#' @export
list_jumpstart_frameworks = function(filter = Constant$new(BooleanValues$`TRUE`),
                                     region = JUMPSTART_DEFAULT_REGION_NAME()){
  frameworks = lapply(
    .generate_jumpstart_model_versions(filter=filter, region=region),
    function(ll){
      fw_ll = extract_framework_task_model(ll$model_id)
      return(fw_ll[[1]])
  })
  return(frameworks)
}

#' @title List scripts for JumpStart, and optionally apply filters to result.
#' @param filter (Union[Operator, str]): Optional. The filter to apply to list scripts. This can be
#'              either an ``Operator`` type filter (e.g. ``And("task == ic", "framework == pytorch")``),
#'              or simply a string filter which will get serialized into an Identity filter.
#'              (e.g. ``"task == ic"``). If this argument is not supplied, all scripts will be listed.
#'              (Default: \code{Constant(BooleanValues$`TRUE`)}).
#' @param region (str): Optional. The AWS region from which to retrieve JumpStart metadata regarding
#'              models. (Default: \code{JUMPSTART_DEFAULT_REGION_NAME()}).
#' @export
list_jumpstart_scripts = function(filter = Constant$new(BooleanValues$`TRUE`),
                                  region = JUMPSTART_DEFAULT_REGION_NAME()){
  if ((inherits(filter, "Constant") && filter$resolved_value == BooleanValues$`TRUE`) ||
      (is.character(filter) && tolower(filter) == tolower(BooleanValues$`TRUE`))){
    jsss = as.list(JumpStartScriptScope)
    return(jsss[order(names(jsss))])
  }


  scripts = list()
  for (ll in .generate_jumpstart_model_versions(filter=filter, region=region)) {
    scripts[length(scripts) + 1 ] = JumpStartScriptScope$INFERENCE
    model_specs = JumpStartModelsAccessor$get_model_specs(
      region=region,
      model_id=ll$model_id,
      version=ll$version
    )
    if (model_specs$training_supported)
      scripts[length(scripts) + 1 ] = JumpStartScriptScope$TRAINING

    scripts = unique(scripts)

    if (identical(scripts, unname(unlist(JumpStartScriptScope))))
      break
  }
}

# NOTE: ensure .generate_jumpstart_model_versions returns list(model_id = ", version="")

#' @title Generate models for JumpStart, and optionally apply filters to result.
#' @param filter (Union[Operator, str]): Optional. The filter to apply to generate models. This can be
#'              either an ``Operator`` type filter (e.g. ``And("task == ic", "framework == pytorch")``),
#'              or simply a string filter which will get serialized into an Identity filter.
#'              (e.g. ``"task == ic"``). If this argument is not supplied, all models will be generated.
#'              (Default: Constant(BooleanValues$TRUE)).
#' @param region (str): Optional. The AWS region from which to retrieve JumpStart metadata regarding
#'              models. (Default: JUMPSTART_DEFAULT_REGION_NAME()).
#' @param list_incomplete_models (bool): Optional. If a model does not contain metadata fields
#'              requested by the filter, and the filter cannot be resolved to a include/not include,
#'              whether the model should be included. By default, these models are omitted from
#'              results. (Default: False).
.generate_jumpstart_model_versions = function(filter = Constant$new(BooleanValues$`TRUE`),
                                              region = JUMPSTART_DEFAULT_REGION_NAME(),
                                              list_incomplete_models = FALSE){
  if (is.character(filter))
    filter = Identity$new(filter)

  models_manifest_list = JumpStartModelsAccessor$get_manifest(region=region)
  manifest_keys = models_manifest_list[[1]]$.__enclos_env__$private$.slots

  all_keys = listenv()
  model_filters = listenv()

  for (operator in .model_filter_in_operator_generator(filter)){
    model_filter = operator$unresolved_value
    key = model_filter$key
    listenv.append(all_keys, key)
    listenv.append(model_filters, model_filter)
  }

  for (key in as.list(all_keys)) {
    if (grepl(".", key))
      NotImplementedError$new(sprintf(
        "No support for multiple level metadata indexing ('%s').", key
      ))
  }
  metadata_filter_keys = as.list(all_keys)[!(as.list(all_keys) %in% SPECIAL_SUPPORTED_FILTER_KEYS)]

  required_manifest_keys = intersect(manifest_keys, metadata_filter_keys)
  possible_spec_keys = metadata_filter_keys[!(metadata_filter_keys %in% manifest_keys)]

  unrecognized_keys = listenv()

  is_task_filter = SpecialSupportedFilterKeys$TASK %in% all_keys
  is_framework_filter = SpecialSupportedFilterKeys$FRAMEWORK %in% all_keys
  is_supported_model_filter = SpecialSupportedFilterKeys$SUPPORTED_MODEL %in% all_keys

  for (model_manifest in models_manifest_list){

    copied_filter = filter

    manifest_specs_cached_values = list()

    model_filters_to_resolved_values = list()

    for (val in required_manifest_keys){
      manifest_specs_cached_values[[val]] = model_manifest[[val]]
    }

    if (is_task_filter){
      manifest_specs_cached_values[[
        SpecialSupportedFilterKeys$TASK
      ]] = extract_framework_task_model(model_manifest$model_id)[[2]]
    }
    if (is_framework_filter){
      manifest_specs_cached_values[[
        SpecialSupportedFilterKeys$FRAMEWORK
      ]] = extract_framework_task_model(model_manifest$model_id)[[1]]
    }
    if (is_supported_model_filter){
      manifest_specs_cached_values[SpecialSupportedFilterKeys$SUPPORTED_MODEL] = numeric_version(
        model_manifest$min_version
      ) <= numeric_version(get_sagemaker_version())
    }
    .populate_model_filters_to_resolved_values(
      manifest_specs_cached_values,
      model_filters_to_resolved_values,
      model_filters
    )

    .put_resolved_booleans_into_filter(copied_filter, model_filters_to_resolved_values)

    copied_filter$eval()

    if (copied_filter$resolved_value %in% list(BooleanValues$`TRUE`, BooleanValues$`FALSE`)){
      if (copied_filter$resolved_value == BooleanValues$`TRUE`)
        # double check this part
        return(model_id = model_manifest$model_id, version = model_manifest$version)
      next
    }

    if (copied_filter$resolved_value == BooleanValues$UNEVALUATED){
      RuntimeError$new(
        "Filter expression in unevaluated state after using values from model manifest. ",
        "Model ID and version that is failing: ",
        sprintf("(%s, %s).", model_manifest$model_id, model_manifest$version)
      )
    }
    copied_filter_2 = filter

    model_specs = JumpStartModelsAccessor$get_model_specs(
      region=region,
      model_id=model_manifest$model_id,
      version=model_manifest$version
    )

    model_specs_keys = model_specs$.__enclos_env__$private$.slots

    modifyListenv(
      unrecognized_keys,
      setNames(rep(list(NULL), length(model_specs_keys)), names(model_specs_keys))
    )

    unrecognized_keys_for_single_spec = possible_spec_keys[!(possible_spec_keys %in% model_specs_keys)]
    modifyListenv(unrecognized_keys, unrecognized_keys_for_single_spec)

    for (val in possible_spec_keys) {
      if (!is.null(model_specs[[val]])){
        manifest_specs_cached_values[[val]] = model_specs[[val]]
      }
    }
    .populate_model_filters_to_resolved_values(
      manifest_specs_cached_values,
      model_filters_to_resolved_values,
      model_filters
    )
    .put_resolved_booleans_into_filter(copied_filter_2, model_filters_to_resolved_values)

    copied_filter_2$eval()

    if (copied_filter_2$resolved_value != BooleanValues$UNEVALUATED){
      if (copied_filter_2$resolved_value == BooleanValues$`TRUE` || (
        BooleanValues$UNKNOWN && list_incomplete_models)){
        return(list(model_id = model_manifest$model_id, version = model_manifest$version))
      }
      next
    }

    RuntimeError$new(
      "Filter expression in unevaluated state after using values from model specs. ",
      "Model ID and version that is failing: ",
      sprintf("(%s, %s).", model_manifest$model_id, model_manifest$version)
    )
  }
  if (length(unrecognized_keys) > 0)
    RuntimeError$new(sprintf("Unrecognized keys: %s", as.character(unrecognized_keys)))
}

#' @title Retrieve web url describing pretrained model.
#' @param model_id (str): The model ID for which to retrieve the url.
#' @param model_version (str): The model version for which to retrieve the url.
#' @param region (str): Optional. The region from which to retrieve metadata.
#'              (Default: JUMPSTART_DEFAULT_REGION_NAME())
#' @export
get_model_url = function(model_id,
                         model_version,
                         region = JUMPSTART_DEFAULT_REGION_NAME()){
  model_specs = JumpStartModelsAccessor$get_model_specs(
    region=region, model_id=model_id, version=model_version
  )
  return(model_specs$url)
}



