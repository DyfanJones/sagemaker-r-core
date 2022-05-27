# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/cache.py

#' @include jumpstart_constants.R
#' @include jumpstart_parameters.R
#' @include jumpstart_types.R
#' @include jumpstart_utils.R
#' @include utilities_cache.R
#' @include r_utils.R

#' @import R6
#' @import paws
#' @importFrom jsonlite fromJSON

#' @title Class that implements a cache for JumpStart models manifests and specs.
#' @description The manifest and specs associated with JumpStart models provide the information necessary
#'              for launching JumpStart models from the SageMaker SDK.
#' @export
JumpStartModelsCache = R6Class("JumpStartModelsCache",
  public = list(

    #' @description Initialize a ``JumpStartModelsCache`` instance.
    #' @param region (str): AWS region to associate with cache. Default: region associated
    #'              with boto3 session.
    #' @param max_s3_cache_items (int): Maximum number of items to store in s3 cache.
    #'              Default: 20.
    #' @param s3_cache_expiration_horizon (datetime.timedelta): Maximum time to hold
    #'              items in s3 cache before invalidation. Default: 6 hours.
    #' @param max_semantic_version_cache_items (int): Maximum number of items to store in
    #'              semantic version cache. Default: 20.
    #' @param semantic_version_cache_expiration_horizon (datetime.timedelta):
    #'              Maximum time to hold items in semantic version cache before invalidation.
    #'              Default: 6 hours.
    #' @param manifest_file_s3_key (str): The key in S3 corresponding to the sdk metadata manifest.
    #' @param s3_bucket_name (Optional[str]): S3 bucket to associate with cache.
    #'              Default: JumpStart-hosted content bucket for region.
    initialize = function(region=JUMPSTART_DEFAULT_REGION_NAME,
                          max_s3_cache_items=JUMPSTART_DEFAULT_MAX_S3_CACHE_ITEMS,
                          s3_cache_expiration_horizon=JUMPSTART_DEFAULT_S3_CACHE_EXPIRATION_HORIZON,
                          max_semantic_version_cache_items=JUMPSTART_DEFAULT_MAX_SEMANTIC_VERSION_CACHE_ITEMS,
                          semantic_version_cache_expiration_horizon=JUMPSTART_DEFAULT_SEMANTIC_VERSION_CACHE_EXPIRATION_HORIZON,
                          manifest_file_s3_key=JUMPSTART_DEFAULT_MANIFEST_FILE_S3_KEY,
                          s3_bucket_name=NULL){
      private$.region = region
      private$.s3_cache = LRUCache$new(
        max_cache_items=max_s3_cache_items,
        expiration_horizon=s3_cache_expiration_horizon,
        retrieval_function=private$.get_file_from_s3
      )
      private$.model_id_semantic_version_manifest_key_cache = LRUCache$new(
        max_cache_items=max_semantic_version_cache_items,
        expiration_horizon=semantic_version_cache_expiration_horizon,
        retrieval_function=private$.get_manifest_key_from_model_id_semantic_version
      )
      private$.manifest_file_s3_key = manifest_file_s3_key
      self$s3_bucket_name = (
        if (is.null(s3_bucket_name))
          get_jumpstart_content_bucket(private$.region)
        else s3_bucket_name
      )
      private$.s3_client = paws::s3(config = list(region=private$.region))
    },

    #' @description Set region for cache. Clears cache after new region is set.
    #' @param region AWS region to associate with cache.
    set_region = function(region){
      if (region != private$.region){
        private$.region = region
        self$clear()
      }
    },

    #' @description Return region for cache.
    get_region = function(){
      return(private$.region)
    },

    #' @description Set manifest file s3 key. Clears cache after new key is set.
    #' @param key (str): The key in S3 corresponding to the sdk metadata manifest.
    set_manifest_file_s3_key = function(key){
      if (key != private$.manifest_file_s3_key){
        private$.manifest_file_s3_key = key
        self$clear()
      }
    },

    #' @description Return manifest file s3 key for cache.
    get_manifest_file_s3_key = function(){
      return(private$.manifest_file_s3_key)
    },

    #' @description Set s3 bucket used for cache.
    #' @param s3_bucket_name (str): S3 bucket to associate with cache.
    set_s3_bucket_name = function(){
      if (s3_bucket_name != self$s3_bucket_name){
        self$s3_bucket_name = s3_bucket_name
        self$clear()
      }
    },

    #' @description Return bucket used for cache.
    get_bucket = function(){
      return(self$s3_bucket_name)
    },

    #' @description Return entire JumpStart models manifest.
    get_manifest = function(){
      manifest_dict = private$.s3_cache$get(
        JumpStartCachedS3ContentKey$new(JumpStartS3FileType$MANIFEST, private$.manifest_file_s3_key)
      )$formatted_content
      manifest = as.list(unname(manifest_dict$values()))
      return(manifest)
    },

    #' @description Return header for a given JumpStart model ID and semantic version.
    #' @param model_id (str): model ID for which to get a header.
    #' @param semantic_version_str (str): The semantic version for which to get a
    #'              header.
    get_header = function(){
      return(private$.get_header_impl(model_id, semantic_version_str=semantic_version_str))
    },

    #' @description Return specs for a given JumpStart model ID and semantic version.
    #' @param model_id (str): model ID for which to get specs.
    #' @param semantic_version_str (str): The semantic version for which to get
    #'              specs.
    get_specs = function(){
      header = self$get_header(model_id, semantic_version_str)
      spec_key = header$spec_key
      specs = private$.s3_cache$get(
        JumpStartCachedS3ContentKey$new(JumpStartS3FileType$SPECS, spec_key)
      )$formatted_content
      return(specs)  # type: ignore
    },

    #' @description Clears the model ID/version and s3 cache.
    clear = function(){
      private$.s3_cache$clear()
      private$.model_id_semantic_version_manifest_key_cache$clear()
    }
  ),
  private = list(
    .region = NULL,
    .s3_cache = NULL,
    .manifest_file_s3_key = NULL,

    # Return model ID and version in manifest that matches semantic version/id.
    # Uses ``packaging.version`` to perform version comparison. The highest model version
    # matching the semantic version is used, which is compatible with the SageMaker
    # version.
    # Args:
    #   key (JumpStartVersionedModelId): Key for which to fetch versioned model ID.
    # value (Optional[JumpStartVersionedModelId]): Unused variable for current value of
    # old cached model ID/version.
    .get_manifest_key_from_model_id_semantic_version = function(key,
                                                                value=NULL){
      model_id = key$model_id
      version = key$version

      manifest = private$.s3_cache$get(
        JumpStartCachedS3ContentKey$new(JumpStartS3FileType$MANIFEST, private$.manifest_file_s3_key)
      )$formatted_content

      sm_version = get_sagemaker_version()

      versions_compatible_with_sagemaker = Filter(Negate(is.null), lapply(
        manifest$values(), function(header){
          if (header$model_id == model_id && numeric_version(header$min_version) <= numeric_version(sm_version))
            numeric_version(header$version)
      }))

      sm_compatible_model_version = private$.select_version(
        version, versions_compatible_with_sagemaker
      )

      if (!is.null(sm_compatible_model_version)){
        return(JumpStartVersionedModelId$new(model_id, sm_compatible_model_version))
      }

      versions_incompatible_with_sagemaker = Filter(Negate(is.null), lapply(
        manifest$values(), function(header){
          if (header$model_id == model_id)
            numeric_version(header$version)
      }))
      sm_incompatible_model_version = private$.select_version(
        version, versions_incompatible_with_sagemaker
      )
      if (!is.null(sm_incompatible_model_version)){
        model_version_to_use_incompatible_with_sagemaker = sm_incompatible_model_version
        sm_version_to_use_list = Filter(Negate(is.null), lapply(
          manifest$values(), function(header){
            if (header$model_id == model_id
               && header$version == model_version_to_use_incompatible_with_sagemaker)
              header$min_version
        }))
        if (len(sm_version_to_use_list) != 1){
          # ``manifest`` dict should already enforce this
          RuntimeError$new("Found more than one incompatible SageMaker version to use.")
        }
        sm_version_to_use = sm_version_to_use_list[[1]]

        error_msg = paste(
          sprintf("Unable to find model manifest for '%s' with version '%s'", model_id, version),
          sprintf("compatible with your SageMaker version ('%s').", sm_version ),
          "Consider upgrading your SageMaker library to at least version",
          sprintf("'%s' so you can use version", sm_version_to_use),
          sprintf("'%s' of '%s'.", model_version_to_use_incompatible_with_sagemaker, model_id)
        )
        KeyError$new(error_msg)
      }
      error_msg = paste(
        spintf("Unable to find model manifest for '%s' with version '%s'. ", model_id, version),
        "Visit https://sagemaker.readthedocs.io/en/stable/doc_utils/jumpstart.html",
        "for updated list of models."
      )
      other_model_id_version = private$.select_version(
        "*", versions_incompatible_with_sagemaker
      )  # all versions here are incompatible with sagemaker
      if (!is.null(other_model_id_version)) {
        error_msg = paste(error_msg,
          sprintf("Consider using model ID '%s' with version", model_id),
          other_model_id_version
        )
      } else {
        possible_model_ids = lapply(manifest$values(), function(header){header$model_id})
        closest_model_id = get_close_matches(model_id, possible_model_ids, n=1, cutoff=0)[1]
        error_msg = paste(error_msg,
          sprintf("Did you mean to use model ID '%s'?", closest_model_id)
        )
      }
      KeyError$new(error_msg)
    },

    # Return s3 content given a file type and s3_key in ``JumpStartCachedS3ContentKey``.
    # If a manifest file is being fetched, we only download the object if the md5 hash in
    # ``head_object`` does not match the current md5 hash for the stored value. This prevents
    # unnecessarily downloading the full manifest when it hasn't changed.
    # Args:
    # key (JumpStartCachedS3ContentKey): key for which to fetch s3 content.
    # value (Optional[JumpStartVersionedModelId]): Current value of old cached
    # s3 content. This is used for the manifest file, so that it is only
    # downloaded when its content changes.
    .get_file_from_s3 = function(key,
                                 value){
      file_type = key$file_type
      s3_key = key$s3_key

      if (file_type == JumpStartS3FileType$MANIFEST){
        if (!is.null(value)){
          etag = private$.s3_client$head_object(Bucket=self$s3_bucket_name, Key=s3_key)[["ETag"]]
          if (etag == value$md5_hash)
            return(value)
        }
        response = private$.s3_client$get_object(Bucket=self$s3_bucket_name, Key=s3_key)
        formatted_body = jsonlite::fromJSON(enc2utf8(readBin(response[["Body"]], character())))
        etag = response[["ETag"]]
        return (JumpStartCachedS3ContentValue$new(
          formatted_content=get_formatted_manifest(formatted_body),
          md5_hash=etag)
        )
      }
      if (file_type == JumpStartS3FileType$SPECS) {
        response = private$.s3_client$get_object(Bucket=self$s3_bucket_name, Key=s3_key)
        formatted_body = jsonlite::fromJSON(enc2utf8(readBin(response[["Body"]], character())))
        return(JumpStartCachedS3ContentValue$new(
          formatted_content=JumpStartModelSpecs(formatted_body))
        )
      }
      ValueError$new(spintf(
        "Bad value for key '%s': must be in [%s, %s]",key , JumpStartS3FileType$MANIFEST, JumpStartS3FileType$SPECS)
      )
    },

    # Perform semantic version search on available versions.
    # Args:
    #   semantic_version_str (str): the semantic version for which to filter
    # available versions.
    # available_versions (List[Version]): list of available versions
    .select_version = function(semantic_version_str,
                               available_versions){
      if (semantic_version_str == "*"){
        if (length(available_versions) == 0)
          return(NULL)
        return(as.character(max(available_versions)))
      }
      available_versions_filtered = available_versions[
        numeric_version(sapply(available_versions, as.character)) == numeric_version(semantic_version_str)
      ]
      return (
        if (length(available_versions_filtered) != 0)
         as.character(max(available_versions_filtered))
        else NULL
      )
    },

    # Lower-level function to return header.
    # Allows a single retry if the cache is old.
    # Args:
    #   model_id (str): model ID for which to get a header.
    # semantic_version_str (str): The semantic version for which to get a
    # header.
    # attempt (int): attempt number at retrieving a header.
    .get_header_impl = function(model_id,
                                semantic_version_str,
                                attempt = 0){
      versioned_model_id = private$.model_id_semantic_version_manifest_key_cache$get(
        JumpStartVersionedModelId$new(model_id, semantic_version_str)
      )
      manifest = private$.s3_cache$get(
        JumpStartCachedS3ContentKey$new(JumpStartS3FileType$MANIFEST, private$.manifest_file_s3_key)
      )$formatted_content

      header = manifest[[versioned_model_id]]
      if (!is.null(header)) {
        return(header)
      } else {
        if (attempt > 0){
          SagemakerError$new()
        }
        self$clear()
        return(private$.get_header_impl(model_id, semantic_version_str, attempt + 1))
      }
    }
  ),
  lock_objects = F
)

