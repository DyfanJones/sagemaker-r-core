# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/image_uris.py

#' @include r_utils.R
#' @include utils.R
#' @include error.R
#' @include workflow_utils.R

#' @import jsonlite
#' @import R6
#' @import lgr

#' @title ImageUris Class
#' @description Class to create and format sagemaker docker images stored in ECR
#' @export
ImageUris = R6Class("ImageUris",
  public = list(

    #' @description Retrieves the ECR URI for the Docker image matching the given arguments of inbuilt AWS Sagemaker models.
    #' @param framework (str): The name of the framework or algorithm.
    #' @param region (str): The AWS region.
    #' @param version (str): The framework or algorithm version. This is required if there is
    #'              more than one supported version for the given framework or algorithm.
    #' @param py_version (str): The Python version. This is required if there is
    #'              more than one supported Python version for the given framework version.
    #' @param instance_type (str): The SageMaker instance type. For supported types, see
    #'              https://aws.amazon.com/sagemaker/pricing/instance-types. This is required if
    #'              there are different images for different processor types.
    #' @param accelerator_type (str): Elastic Inference accelerator type. For more, see
    #'              https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html.
    #' @param image_scope (str): The image type, i.e. what it is used for.
    #'              Valid values: "training", "inference", "eia". If ``accelerator_type`` is set,
    #'              ``image_scope`` is ignored.
    #' @param container_version (str): the version of docker image
    #' @param distribution (dict): A dictionary with information on how to run distributed training
    #'              (default: None).
    #' @param base_framework_version (str):
    #' @param training_compiler_config (:class:`~sagemaker.training_compiler.TrainingCompilerConfig`):
    #'              A configuration class for the SageMaker Training Compiler
    #'              (default: None).
    #' @param model_id (str): The JumpStart model ID for which to retrieve the image URI
    #'              (default: None).
    #' @param model_version (str): The version of the JumpStart model for which to retrieve the
    #'              image URI (default: None).
    #' @param tolerate_vulnerable_model (bool): ``True`` if vulnerable versions of model specifications
    #'              should be tolerated without an exception raised. If ``False``, raises an exception if
    #'              the script used by this version of the model has dependencies with known security
    #'              vulnerabilities. (Default: False).
    #' @param tolerate_deprecated_model (bool): True if deprecated versions of model specifications
    #'              should be tolerated without an exception raised. If False, raises an exception
    #'              if the version of the model is deprecated. (Default: False).
    #' @param sdk_version (str): the version of python-sdk that will be used in the image retrieval.
    #'              (default: None).
    #' @param inference_tool (str): the tool that will be used to aid in the inference.
    #'              Valid values: "neuron, None" (default: None).
    #' @param serverless_inference_config (\code{sagemaker.core::ServerlessInferenceConfig}):
    #'              Specifies configuration related to serverless endpoint. Instance type is
    #'              not provided in serverless inference. So this is used to determine processor type.
    #' @return str: the ECR URI for the corresponding SageMaker Docker image.
    retrieve = function(framework,
                        region,
                        version=NULL,
                        py_version=NULL,
                        instance_type=NULL,
                        accelerator_type=NULL,
                        image_scope=NULL,
                        container_version=NULL,
                        distribution=NULL,
                        base_framework_version=NULL,
                        training_compiler_config=NULL,
                        model_id=NULL,
                        model_version=NULL,
                        tolerate_vulnerable_model=FALSE,
                        tolerate_deprecated_model=FALSE,
                        sdk_version=NULL,
                        inference_tool=NULL,
                        serverless_inference_config=NULL){
      args = as.list(environment())

      for (name in args){
        val = args[[name]]
        if (is_pipeline_variable(val))
          ValueError$new(sprintf(
            "%s should not be a pipeline variable (%s)", name, class(val)
          ))
      }
      if (is_jumpstart_model_input(model_id, model_version)){
        return(.retrieve_image_uri(
          model_id,
          model_version,
          image_scope,
          framework,
          region,
          version,
          py_version,
          instance_type,
          accelerator_type,
          container_version,
          distribution,
          base_framework_version,
          training_compiler_config,
          tolerate_vulnerable_model,
          tolerate_deprecated_model
        ))
      }

      if (is.null(training_compiler_config)) {
        .framework = framework
        if (framework == private$HUGGING_FACE_FRAMEWORK) {
          inference_tool = private$.get_inference_tool(inference_tool, instance_type)
          if (inference_tool == "neuron") {
            .framework = sprintf("%s-%s", framework, inference_tool)
            config = private$.config_for_framework_and_scope(.framework, image_scope, accelerator_type)
          }
        }
      } else if (framework == private$HUGGING_FACE_FRAMEWORK) {
        config = private$.config_for_framework_and_scope(
          paste0(framework, "-training-compiler"), image_scope, accelerator_type
        )
      } else {
        ValueError$new(
          "Unsupported Configuration: Training Compiler is only supported with HuggingFace"
        )
      }

      original_version = version
      version = private$.validate_version_and_set_if_needed(version, config, framework)

      # Read dictionary key "" as position instead due to how jsonlite reads in jsons
      version_config = private$.version_for_config(version, config)
      version_config = config[["versions"]][[(if (identical(version_config, "")) 1L else version_config)]]

      if(framework == private$HUGGING_FACE_FRAMEWORK){
        if (!islistempty(version_config[["version_aliases"]])){
          full_base_framework_version = version_config[["version_aliases"]][[
            base_framework_version]] %||% base_framework_version
        }
        private$.validate_arg(full_base_framework_version, names(version_config), "base framework")
        version_config = version_config[[full_base_framework_version]]
      }

      py_version = private$.validate_py_version_and_set_if_needed(py_version, version_config, framework)
      version_config = if(is.null(py_version)) version_config else {version_config[[py_version]] %||% version_config}
      registry = private$.registry_from_region(region, version_config$registries)
      hostname = regional_hostname("ecr", region)

      repo = version_config[["repository"]]

      processor = private$.processor(
        instance_type=instance_type,
        available_processors=(config[["processors"]] %||% version_config[["processors"]]),
        serverless_inference_config
      )


      # if container version is available in .json file, utilize that
      if (!is.null(version_config[["container_version"]]))
        container_version = version_config[["container_version"]][[processor]]

      if(framework == private$HUGGING_FACE_FRAMEWORK){
        pt_or_tf_version = private$.str_match(base_framework_version, "^(pytorch|tensorflow)(.*)$")[[3]]

        .version = original_version

        if (repo %in% c("huggingface-pytorch-trcomp-training", "huggingface-tensorflow-trcomp-training")){
          .version = version

          if(repo %in% c("huggingface-pytorch-inference-neuron")){
            if (is.null(sdk_version))
              sdk_version = .get_latest_versions(version_config[["sdk_versions"]])
            container_version = paste0(sdk_version, "-", container_version)
            if (!is.null(config[["version_aliases"]][[original_version]]))
              .version = config[["version_aliases"]][[original_version]]
            if (
              !is.null(config[["versions"]][[
                .version]][[
                "version_aliases"]][[
                base_framework_version]])
            ){
              .base_framework_version = config[["versions"]][[.version]][["version_aliases"]][[
                base_framework_version
              ]]
              pt_or_tf_version =  private$.str_match(
                .base_framework_version, "^(pytorch|tensorflow)(.*)$"
              )[[3]]
            }
          }
        }
        tag_prefix = sprintf("%s-transformers%s", pt_or_tf_version, .version)
      } else {
        tag_prefix = version_config[["tag_prefix"]] %||% version
      }

      tag = private$.format_tag(
        tag_prefix, processor, py_version, container_version, inference_tool
      )

      if(private$.should_auto_select_container_version(instance_type, distribution)){
        container_versions = list(
          "tensorflow-2.3-gpu-py37"="cu110-ubuntu18.04-v3",
          "tensorflow-2.3.1-gpu-py37"="cu110-ubuntu18.04",
          "tensorflow-2.3.2-gpu-py37"="cu110-ubuntu18.04",
          "tensorflow-1.15-gpu-py37"="cu110-ubuntu18.04-v8",
          "tensorflow-1.15.4-gpu-py37"="cu110-ubuntu18.04",
          "tensorflow-1.15.5-gpu-py37"="cu110-ubuntu18.04",
          "mxnet-1.8-gpu-py37"="cu110-ubuntu16.04-v1",
          "mxnet-1.8.0-gpu-py37"="cu110-ubuntu16.04",
          "pytorch-1.6-gpu-py36"="cu110-ubuntu18.04-v3",
          "pytorch-1.6.0-gpu-py36"="cu110-ubuntu18.04",
          "pytorch-1.6-gpu-py3"="cu110-ubuntu18.04-v3",
          "pytorch-1.6.0-gpu-py3"="cu110-ubuntu18.04"
        )
        key = paste(list(framework, tag), collapse = "-", sep= "-")
        if (key %in% names(container_versions))
          tag = paste(list(tag, container_versions[[key]]), collapse = "-", sep = "-")
      }

      if(!is.null(tag))
        repo = sprintf("%s:%s", repo, tag)

      return(sprintf(private$ECR_URI_TEMPLATE, registry, hostname, repo))
    },

    #' @description Retrieves the image URI for training.
    #' @param region (str): The AWS region to use for image URI.
    #' @param framework (str): The framework for which to retrieve an image URI.
    #' @param framework_version (str): The framework version for which to retrieve an
    #'              image URI (default: NULL).
    #' @param py_version (str): The python version to use for the image (default: NULL).
    #' @param image_uri (str): If an image URI is supplied, it is returned (default: NULL).
    #' @param distribution (dict): A dictionary with information on how to run distributed
    #'              training (default: NULL).
    #' @param compiler_config (:class:`~sagemaker.training_compiler.TrainingCompilerConfig`):
    #'              A configuration class for the SageMaker Training Compiler
    #'              (default: NULL).
    #' @param tensorflow_version (str): The version of TensorFlow to use. (default: NULL)
    #' @param pytorch_version (str): The version of PyTorch to use. (default: NULL)
    #' @param instance_type (str): The instance type to use. (default: NULL)
    #' @return str: The image URI string.
    get_training_image_uri = function(region,
                                      framework,
                                      framework_version=NULL,
                                      py_version=NULL,
                                      image_uri=NULL,
                                      distribution=NULL,
                                      compiler_config=NULL,
                                      tensorflow_version=NULL,
                                      pytorch_version=NULL,
                                      instance_type=NULL){
      if (!is.null(image_uri))
        return(image_uri)

      base_framework_version = NULL

      if (!is.null(tensorflow_version) || !is.null(pytorch_version)) {
        processor = private$.processor(instance_type, c("cpu", "gpu"))
        is_native_huggingface_gpu = (processor == "gpu" && !is.null(compiler_config))
        container_version = if (is_native_huggingface_gpu) "cu110-ubuntu18.04" else NULL
        if (!is.null(tensorflow_version)) {
          base_framework_version = sprintf("tensorflow%s", tensorflow_version)
        } else {
          base_framework_version = sprintf("pytorch%s", pytorch_version)
        }
      } else {
        container_version = NULL
        base_framework_version = NULL
      }
      return(self$retrieve(
        framework,
        region,
        instance_type=instance_type,
        version=framework_version,
        py_version=py_version,
        image_scope="training",
        distribution=distribution,
        base_framework_version=base_framework_version,
        container_version=container_version,
        training_compiler_config=compiler_config
      ))
    },

    #' @description format class
    format = function(){
      return(format_class(self))
    }
  ),

  private = list(
    ECR_URI_TEMPLATE = "%s.dkr.%s/%s",
    HUGGING_FACE_FRAMEWORK = "huggingface",

    # Loads the JSON config for the given framework and image scope.
    .config_for_framework_and_scope = function(framework,
                                               image_scope = NULL,
                                               accelerator_type=NULL){
      config = config_for_framework(framework)

      if (!is.null(accelerator_type)){
        private$.validate_accelerator_type(accelerator_type)

        if (!(is.null(image_scope) || image_scope %in% c("eia", "inference")))
          LOGGER$warn(
            "Elastic inference is for inference only. Ignoring image scope: %s.", image_scope
          )
        image_scope = "eia"
      }

      available_scopes = if(!islistempty(config$scope)) config$scope else names(config)

      if (length(available_scopes) == 1){
        if (!islistempty(image_scope) && image_scope != available_scopes[[1]])
          LOGGER$warn(
            "Defaulting to only supported image scope: %s. Ignoring image scope: %s.",
            available_scopes[1],
            image_scope
          )
        image_scope = available_scopes[[1]]
      }
      if (islistempty(image_scope) && "scope" %in% names(config) && any(unique(available_scopes) %in% c("training", "inference"))){
        LOGGER$info(
          "Same images used for training and inference. Defaulting to image scope: %s.",
          available_scopes[[1]])
        image_scope = available_scopes[[1]]
      }
      private$.validate_arg(image_scope, available_scopes, "image scope")
      return(if("scope" %in% names(config)) config else config[[image_scope]])
    },

    # Extract the inference tool name from instance type.
    .get_inference_tool = function(inference_tool, instance_type){
      if (missing(inference_tool) && !missing(instance_type)){
        match = private$.str_match("^ml[\\._]([a-z\\d]+)\\.?\\w*$", instance_type)
        if (!islistempty(match) && grepl("^inf", match[[2]]))
          return("neuron")
      }
      return(inference_tool)
    },

    # Extract the latest version from the input list of available versions.
    .get_latest_versions = function(list_of_versions){
      return(sort(as.character(list_of_versions), decreasing = T)[1])
    },

    # Raises a ``ValueError`` if ``accelerator_type`` is invalid.
    .validate_accelerator_type = function(accelerator_type){
      if (!startsWith(accelerator_type, "ml.eia") && accelerator_type != "local_sagemaker_notebook")
        ValueError$new(sprintf(
          "Invalid SageMaker Elastic Inference accelerator type: %s. ",accelerator_type),
          "See https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html"
        )
    },

    # Checks if the framework/algorithm version is one of the supported versions.
    .validate_version_and_set_if_needed = function(version = NULL,
                                                   config,
                                                   framework){
      available_versions = names(config$versions)
      aliased_versions = names(config$version_aliases) %||% list()
      version = version %||% NA

      if (length(available_versions) == 1 && !(version %in% aliased_versions)){
        log_message = sprintf("Defaulting to the only supported framework/algorithm version: %s.", available_versions[[1]])
        if (!is.na(version) && version != available_versions[[1]])
          LOGGER$warn("%s Ignoring framework/algorithm version: %s.", log_message, version)
        if (is.na(version)){
          LOGGER$info(log_message)
        }
        return(available_versions[[1]])
      }

      private$.validate_arg(
        if(is.na(version)) NULL else version,
        c(available_versions, aliased_versions),
        sprintf("%s version",framework))
      return(version)
    },

    # Returns the version string for retrieving a framework version's specific config.
    .version_for_config = function(version,
                                   config){
      if ("version_aliases" %in% names(config)){
        if (version %in% names(config$version_aliases))
          return(config$version_aliases[[version]])
      }
      return(version)
    },

    # Returns the ECR registry (AWS account number) for the given region.
    .registry_from_region = function(region,
                                     registry_dict){
      private$.validate_arg(region, names(registry_dict), "region")
      return(registry_dict[[region]])
    },

    # Returns the processor type for the given instance type.
    .processor = function(instance_type = NULL,
                          available_processors = NULL,
                          serverless_inference_config = NULL){
      if (is.null(available_processors)){
        if(!is.null(instance_type))
          LOGGER$info("Ignoring unnecessary instance type: %s.", instance_type)
        return(NULL)
      }
      if (length(available_processors) == 1 && is.null(instance_type)){
        LOGGER$info("Defaulting to only supported image scope: %s.", available_processors[[1]])
        return(available_processors[[1]])
      }
      if (!is.null(serverless_inference_config)) {
        LOGGER$info("Defaulting to CPU type when using serverless inference")
        return("cpu")
      }
      if (islistempty(instance_type)){
        ValueError$new(
          "Empty SageMaker instance type. For options, see: ",
          "https://aws.amazon.com/sagemaker/pricing/instance-types")
      }
      if (startsWith(instance_type,"local")){
        processor = if(instance_type == "local") "cpu" else "gpu"
      } else {
        # looks for either "ml.<family>.<size>" or "ml_<family>"
        match = private$.str_match(instance_type, "^ml[\\._]([a-z0-9]+)\\.?\\w*$")[[2]]
        if (length(match) != 0){
          family = match

          # For some frameworks, we have optimized images for specific families, e.g c5 or p3.
          # In those cases, we use the family name in the image tag. In other cases, we use
          # 'cpu' or 'gpu'.
          if (family %in% available_processors) {
            processor = family
          } else if (startsWith(family, "inf")) {
            processor = "inf"
          } else if (substr(family, 1,1) %in% c("g", "p")) {
            processor = "gpu"
          } else {
            processor = "cpu"
          }
        } else {
          ValueError$new(sprintf(
            "Invalid SageMaker instance type: %s. For options, see: ", instance_type),
            "https://aws.amazon.com/sagemaker/pricing/instance-types"
          )
        }
      }
      private$.validate_arg(processor, available_processors, "processor")
      return(processor)
    },

    # Returns a boolean that indicates whether to use an auto-selected container version.
    .should_auto_select_container_version = function(instance_type=NULL,
                                                     distribution=NULL){
      p4d = FALSE
      if (!is.null(instance_type)){
        # looks for either "ml.<family>.<size>" or "ml_<family>"
        match = private$.str_match(instance_type, "^ml[\\._]([a-z0-9]+)\\.?\\w*$")
        if (length(match) != 0){
          family = match[2]
          p4d = (family == "p4d")
        }
      }
      smdistributed = FALSE
      if (!islistempty(distribution))
        smdistributed = ("smdistributed" %in% names(distribution))

      return((p4d || smdistributed))
    },

    # # Checks if the Python version is one of the supported versions.
    .validate_py_version_and_set_if_needed = function(py_version,
                                                      version_config,
                                                      framework){
      if ("repository" %in% names(version_config)){
        available_versions = unlist(version_config$py_versions)
      } else {
        available_versions = names(version_config)
      }
      if (islistempty(available_versions)){
        if(!is.null(py_version)){
          LOGGER$info("Ignoring unnecessary Python version: %s.", py_version)}
        return(NULL)
      }
      if (is.null(py_version) && "spark" == framework)
        return(NULL)

      if (is.null(py_version) && length(available_versions) == 1){
        LOGGER$info("Defaulting to only available Python version: %s", available_versions[[1]])
        return(available_versions[[1]])
      }
      private$.validate_arg(py_version, available_versions, "Python version")
      return(py_version)
    },

    # Checks if the arg is in the available options, and raises a ``ValueError`` if not.
    .validate_arg = function(arg, available_options, arg_name){
      if (!(arg %in% available_options) || is.null(arg))
        ValueError$new(sprintf(paste(
          "Unsupported %s: %s. You may need to upgrade your SDK version",
          "(remotes::install_github('DyfanJones/sagemaker-r-common')) for newer %ss.",
          "\nSupported %s(s): {%s}."), arg_name, arg %||% "NULL", arg_name, arg_name,
          paste(available_options, collapse = ", ")
        ))
    },

    # Creates a tag for the image URI.
    .format_tag = function(tag_prefix,
                           processor,
                           py_version,
                           container_version,
                           inference_tool = NULL){
      if (!is.null(inference_tool)) {
        tag_list = list(tag_prefix, inference_tool, py_version, container_version)
        tag_list = Filter(Negate(is.null), tag_list)
        return (paste(tag_list, collapse = "-"))
      }
      tag_list = list(tag_prefix, processor, py_version, container_version)
      tag_list = Filter(Negate(is.null), tag_list)
      return (paste(tag_list, collapse = "-"))
    },

    .str_match = function(string, pattern){
      m = regexec(pattern, string)
      return(unlist(regmatches(string, m)))
    }
  )
)

# Loads the JSON config for the given framework.
config_for_framework = function(framework){
  fname= system.file("image_uri_config", sprintf("%s.json", framework), package=pkg_name())

  # check if framework json file exists first
  if(!file.exists(fname))
    ValueError$new(sprintf(paste(
      "Unsupported framework: %s. You may need to upgrade your SDK version",
      "(remotes::install_github('DyfanJones/sagemaker-r-common')) for newer frameworks."),
      framework))

  return(read_json(fname))
}
