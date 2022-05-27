# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/accessors.py

#' @include jumpstart_types.R
#' @include jumpstart_cache.R
#' @include jumpstart_constants.R
#' @include r_utils.R

#' @import R6

#' @title SageMakerSettings class
#' @description Static class for storing the SageMaker settings.
# @export
.SageMakerSettings = R6Class("SageMakerSettings",
  public = list(

    #' @description Set SageMaker version.
    #' @param version (str): python sagemaker version
    set_sagemaker_version = function(version){
      stopifnot(is.character(version))
      private$.parsed_sagemaker_version = version
    },

    #' @description Return SageMaker version.
    get_sagemaker_version = function(){
      return(private$.parsed_sagemaker_version)
    }
  ),
  private = list(
    .parsed_sagemaker_version = ""
  )
)

#' @title JumpStartModelsAccessor class
#' @description Static class for storing the JumpStart models cache.
# @export
.JumpStartModelsAccessor = R6Class("JumpStartModelsAccessor",
  public = list(

    #' @description Returns model header from JumpStart models cache.
    #' @param region (str): region for which to retrieve header.
    #' @param model_id (str): model id to retrieve.
    #' @param version (str): semantic version to retrieve for the model id.
    get_model_header = function(region,
                                model_id,
                                version){
      cache_kwargs = private$.validate_and_mutate_region_cache_kwargs(
        private$.cache_kwargs, region
      )
      private$.set_cache_and_region(region, cache_kwargs)
      return(private$.cache$get_header(
        model_id=model_id, semantic_version_str=version
      ))
    },

    #' @description  Returns model specs from JumpStart models cache.
    #' @param region (str): region for which to retrieve header.
    #' @param model_id (str): model id to retrieve.
    #' @param version (str): semantic version to retrieve for the model id.
    get_model_specs = function(region,
                               model_id,
                               version){
      cache_kwargs = private$.validate_and_mutate_region_cache_kwargs(
        private$.cache_kwargs, region
      )
      private$.set_cache_and_region(region, cache_kwargs)
      return(private$.cache$get_specs(
        model_id=model_id, semantic_version_str=version
      ))
    },

    #' @description Sets cache kwargs, clears the cache.
    #' @param cache_kwargs (str): cache kwargs to validate.
    #' @param region (str): Optional. The region to validate along with the kwargs.
    set_cache_kwargs = function(cache_kwargs,
                                region=NULL){
      cache_kwargs = private$.validate_and_mutate_region_cache_kwargs(
        cache_kwargs, region
      )
      private$.cache_kwargs = cache_kwargs
      if (is.null(region)){
        private$.cache = do.call(JumpStartModelsCache$new, private$.cache_kwargs)
      } else {
        private$.curr_region = region
        private$.cache = do.call(
          JumpStartModelsCache$new, c(region=region, private$.cache_kwargs)
        )
      }
    },

    #' @description Resets cache, optionally allowing cache kwargs to be passed to the new cache.
    #' @param cache_kwargs (str): cache kwargs to validate.
    #' @param region (str): The region to validate along with the kwargs.
    reset_cache = function(cache_kwargs=NULL, region=NULL){
      cache_kwargs_dict = cache_kwargs %||% list()
      self$set_cache_kwargs(cache_kwargs_dict, region)
    },

    #' @description Return entire JumpStart models manifest.
    #' @param cache_kwargs (Dict[str, Any]): Optional. Cache kwargs to use.
    #'              (Default: None).
    #' @param region (str): Optional. The region to use for the cache.
    #'              (Default: None).
    get_manifest = function(cache_kwargs = NULL,
                            region = NULL){
      cache_kwargs_dict =  cache_kwargs %||% list()
      self$set_cache_kwargs(cache_kwargs_dict, region)
      return(private$.cache$get_manifest())
    }
  ),
  private = list(
    .cache = NULL,
    .curr_region = JUMPSTART_DEFAULT_REGION_NAME,
    .cache_kwargs = list(),

    # Returns cache_kwargs with region argument removed if present.
    # Raises:
    #   ValueError: If region in `cache_kwargs` is inconsistent with `region` argument.
    # Args:
    #   cache_kwargs (Optional[Dict[str, Any]]): cache kwargs to validate.
    # region (str): The region to validate along with the kwargs.
    .validate_and_mutate_region_cache_kwargs = function(cache_kwargs=NULL,
                                                        region=NULL){
      cache_kwargs_dict = cache_kwargs %||% list()
      if (!is.null(region) && "region" %in% names(cache_kwargs_dict)){
        if (region != cache_kwargs_dict["region"]) {
          ValueError$new(sprintf(
            "Inconsistent region definitions: {%s}, {%s}",
            region, cache_kwargs_dict[['region']])
          )
        }
        cache_kwargs_dict[["region"]] = NULL
      }
      return(cache_kwargs_dict)
    },

    # Sets ``JumpStartModelsAccessor._cache`` and ``JumpStartModelsAccessor._curr_region``.
    # Args:
    #   region (str): region for which to retrieve header/spec.
    # cache_kwargs (dict): kwargs to pass to ``JumpStartModelsCache``.
    .set_cache_and_region = function(region,
                                     cache_kwargs){
      if (is.null(private$.cache) || region != private$.curr_region){
        kwargs = c(region=region, cache_kwargs)
        private$.cache = do.call(JumpStartModelsCache$new, kwargs)
        private$.curr_region = region
      }
    }
  )
)

SageMakerSettings = .SageMakerSettings$new()
JumpStartModelsAccessor = .JumpStartModelsAccessor$new()
