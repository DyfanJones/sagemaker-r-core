# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/types.py

#' @import R6
#' @importfrom jsonlite toJSON
#' @importFrom stats setNames
#' @importFrom data.table address

#' @include r_utils.R

JumpStartDataHolderType = R6Class("JumpStartDataHolderType",
  public = list(

    #' @description Returns ``__repr__`` string of object.
    format = function(){
      att_dict = setNames(
        lapply(private$.slots, function(attr) self[[attr]]),
        private$.slots
      )
      return(sprintf(
        "%s at %s: %s",
        class(self)[1],
        data.table::address(self),
        gsub("\"", "'", toJSON(att_dict, auto_unbox = T))
      ))
    }
  ),
  private = list(
    .slots = list()
  )
)


# Returns True if ``other`` is of the same type and has all attributes equal.
# Args:
#   other (Any): Other object to which to compare this object.
#' @export
`==.JumpStartDataHolderType`= function(self, other){
  if (!inherits(other, class(self)))
    return(FALSE)
  if (is.null(other$.__enclos_env__$private$.slots))
    return(FALSE)
  if (!identical(
        self$.__enclos_env__$private$.slots,
        other$.__enclos_env__$private$.slots)
    )
    return(FALSE)
  for (attribute in self$.__enclos_env__$private$.slots){
    if (attribute %in% names(self) && !(attribute %in% names(other)) || (
      attribute %in% names(other) && !(attribute %in% names(self))
    )){
      return(FALSE)
    }
    if (attribute %in% names(self) && attribute %in% names(other)){
      if (self[[attribute]] != other[[attribute]]){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Returns string representation of object. Example:
#   "JumpStartLaunchedRegionInfo:
#         {'content_bucket': 'bucket', 'region_name': 'us-west-2'}"
#' @export
as.character.JumpStartDataHolderType = function(x, ...){
  att_dict = setNames(
    lapply(x$.__enclos_env__$private$.slots, function(attr) x[[attr]]),
    x$.__enclos_env__$private$.slots
  )
  return(sprintf("%s: %s", class(x)[[1]], gsub("\"", "'", toJSON(att_dict, auto_unbox = T))))
}

#' @title Type of files published in JumpStart S3 distribution buckets.
#' @export
JumpStartS3FileType = Enum(
  .class = "JumpStartS3FileType",
  MANIFEST = "manifest",
  SPECS = "specs"
)

#' @title Data class for launched region info.
#' @export
JumpStartLaunchedRegionInfo = R6Class("JumpStartLaunchedRegionInfo",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @field content_bucket
    #' Name of JumpStart s3 content bucket associated with region.
    content_bucket = NULL,

    #' @field region_name
    #' Name of JumpStart launched region.
    region_name = NULL,

    #' @description Instantiates JumpStartLaunchedRegionInfo object.
    #' @param content_bucket (str): Name of JumpStart s3 content bucket associated with region.
    #' @param region_name (str): Name of JumpStart launched region.
    initialize = function(content_bucket,
                          region_name){
      self$content_bucket = content_bucket
      self$region_name = region_name
    }
  ),
  private = list(
    .slots = list("content_bucket", "region_name")
  )
)

#' @title Data class JumpStart model header.
#' @export
JumpStartModelHeader = R6Class("JumpStartModelHeader",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @description Initializes a JumpStartModelHeader object from its json representation.
    #' @param header (Dict[str, str]): Dictionary representation of header.
    initialize = function(header){
      self$from_json(header)
    },

    #' @description Returns json representation of JumpStartModelHeader object in list format.
    to_json = function(){
      json_obj = setNames(
        lapply(private$.slots, function(attr) self[[attr]]),
        private$.slots
      )
      return(json_obj)
    },

    #' @descriptionSets fields in object based on json of header.
    #' @param json_obj (Dict[str, str]): Dictionary representation of header.
    from_json = function(json_obj){
      self$model_id = json_obj[["model_id"]]
      self$version = json_obj[["version"]]
      self$min_version = json_obj[["min_version"]]
      self$spec_key = json_obj[["spec_key"]]
    }
  ),
  private = list(
    .slots = list("model_id", "version", "min_version", "spec_key")
  ),
  lock_objects = F
)

#' @title Data class for JumpStart ECR specs.
#' @export
JumpStartECRSpecs = R6Class("JumpStartECRSpecs",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @description Initializes a JumpStartECRSpecs object from its json representation.
    #' @param spec (Dict[str, Any]): Dictionary representation of spec.
    initialize = function(spec){
      self$from_json(spec)
    },

    #' @description Sets fields in object based on json.
    #' @param json_obj (Dict[str, Any]): Dictionary representation of spec.
    from_json = function(json_obj){
      self$framework = json_obj[["framework"]]
      self$framework_version = json_obj[["framework_version"]]
      self$py_version = json_obj[["py_version"]]
      huggingface_transformers_version = json_obj[["huggingface_transformers_version"]]
      if (!is.null(huggingface_transformers_version))
        self$huggingface_transformers_version = huggingface_transformers_version
    },

    #' @description Returns json representation of JumpStartECRSpecs object in list format.
    to_json = function(){
      json_obj = setNames(
        lapply(private$.slots, function(attr) self[[attr]]),
        private$.slots
      )
      return(json_obj)
    }
  ),
  private = list(
    .slots = list(
      "framework",
      "framework_version",
      "py_version",
      "huggingface_transformers_version"
    )
  ),
  lock_objects = F
)

#' @title JumpStartHyperparameter class
#' @description Data class for JumpStart hyperparameter definition in the training container.
#' @export
JumpStartHyperparameter = R6Class("JumpStartHyperparameter",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @description Initializes a JumpStartHyperparameter object from its json representation.
    #' @param spec (Dict[str, Any]): Dictionary representation of hyperparameter.
    initialize = function(spec){
      self$from_json(spec)
    },

    #' @description Sets fields in object based on json.
    #' @param json_obj (Dict[str, Any]): Dictionary representation of hyperparameter.
    from_json = function(json_obj){

      self$name = json_obj[["name"]]
      self$type = json_obj[["type"]]
      self$default = json_obj[["default"]]
      self$scope = json_obj[["scope"]]

      options = json_obj[["options"]]
      if (!is.null(options))
        self$options = options

      min_val = json_obj[["min"]]
      if (!is.null(min_val))
        self$min = min_val

      max_val = json_obj[["max"]]
      if (!is.null(max_val))
        self$max = max_val

      exclusive_min_val = json_obj[["exclusive_min"]]
      if (is.null(exclusive_min_val))
        self$exclusive_min = exclusive_min_val

      exclusive_max_val = json_obj[["exclusive_max"]]
      if (!is.null(exclusive_max_val))
        self$exclusive_max = exclusive_max_val
    },

    #' @description Returns json representation of JumpStartHyperparameter object.
    to_json = function(){
      json_obj = as.list(self)[names(self) %in% .slots]
      return(json_obj)
    }
  ),
  private = list(
    .slots = c(
      "name",
      "type",
      "options",
      "default",
      "scope",
      "min",
      "max",
      "exclusive_min",
      "exclusive_max"
    )
  ),
  lock_objects = F
)

#' @title JumpStartEnvironmentVariable class
#' @description Data class for JumpStart environment variable definitions in the hosting container.
#' @export
JumpStartEnvironmentVariable = R6Class("JumpStartEnvironmentVariable",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @descriptioon Initializes a JumpStartEnvironmentVariable object from its json representation.
    #' @param spec (Dict[str, Any]): Dictionary representation of environment variable.
    initialize = function(spec){
      self$from_json(spec)
    },

    #' @description Sets fields in object based on json.
    #' @param json_obj (Dict[str, Any]): Dictionary representation of environment variable.
    from_json = function(json_obj){
      self$name = json_obj[["name"]]
      self$type = json_obj[["type"]]
      self$default = json_obj[["default"]]
      self$scope = json_obj[["scope"]]
    },

    #' @description Returns json representation of JumpStartEnvironmentVariable object.
    to_json = function(){
      json_obj = as.list(self)[names(self) %in% .slots]
      return(json_obj)
    }
  ),
  private = list(
    .slots = list(
      "name",
      "type",
      "default",
      "scope"
    )
  ),
  lock_objects = F
)

#' @title JumpStartModelSpecs class
#' @description Data class JumpStart model specs
#' @export
JumpStartModelSpecs = R6Class("JumpStartModelSpecs",
  inherit = ,
  public = list(

    #' @description Initializes a JumpStartModelSpecs object from its json representation.
    #' @param spec (Dict[str, Any]): Dictionary representation of spec.
    initialize = function(spec){
      self$from_json(spec)
    },

    #' @description Sets fields in object based on json of header.
    #' @param json_obj (Dict[str, Any]): Dictionary representation of spec.
    from_json = function(json_obj){
      self$model_id = json_obj[["model_id"]]
      self$url = json_obj[["url"]]
      self$version = json_obj[["version"]]
      self$min_sdk_version = json_obj[["min_sdk_version"]]
      self$incremental_training_supported = as.logical(json_obj[["incremental_training_supported"]])
      self$hosting_ecr_specs = JumpStartECRSpecs$new(json_obj[["hosting_ecr_specs"]])
      self$hosting_artifact_key = json_obj[["hosting_artifact_key"]]
      self$hosting_script_key = json_obj[["hosting_script_key"]]
      self$training_supported = as.logical(json_obj[["training_supported"]])
      self$inference_environment_variables = lapply(
        json_obj[["inference_environment_variables"]], function(env_variable){
          JumpStartEnvironmentVariable$new(env_variable)
      })
      self$inference_vulnerable = as.logical(json_obj[["inference_vulnerable"]])
      self$inference_dependencies = json_obj[["inference_dependencies"]]
      self$inference_vulnerabilities = json_obj[["inference_vulnerabilities"]]
      self$training_vulnerable = as.logical(json_obj[["training_vulnerable"]])
      self$training_dependencies = json_obj[["training_dependencies"]]
      self$training_vulnerabilities = json_obj[["training_vulnerabilities"]]
      self$deprecated = as.logical(json_obj[["deprecated"]])

      if (self$training_supported){
        self$training_ecr_specs = JumpStartECRSpecs$new(
          json_obj[["training_ecr_specs"]]
        )
        self$training_artifact_key = json_obj[["training_artifact_key"]]
        self$training_script_key = json_obj[["training_script_key"]]
        hyperparameters = json_obj[["hyperparameters"]]
        if (!is.null(hyperparameters)){
          self$hyperparameters = lapply(
            hyperparameters, function(hyperparameter){
              JumpStartHyperparameter$new(hyperparameter)
          })
        }
      }
    },

    #' @description Returns json representation of JumpStartModelSpecs object.
    to_json = function(){
      json_obj = list()
      for (att in private$.slots){
        if (!is.null(self[[att]])) {
          cur_val = self[[att]]
          if (inherits(cur_val, "JumpStartDataHolderType")){
            json_obj[[att]] = cur_val$to_json()
          } else if (is.list(cur_val)) {
            ll = list()
              for (obj in cur_val) {
                if (inherits(obj, "JumpStartDataHolderType"))
                  ll = list.append(ll, obj$to_json())
                else
                  ll = list.append(ll, obj)
              }
            json_obj[[att]] = ll
          } else {
            json_obj[[att]] = cur_val
          }
        }
      }
      return(json_obj)
    }
  ),
  private = list(
    .slots = list(
      "model_id",
      "url",
      "version",
      "min_sdk_version",
      "incremental_training_supported",
      "hosting_ecr_specs",
      "hosting_artifact_key",
      "hosting_script_key",
      "training_supported",
      "training_ecr_specs",
      "training_artifact_key",
      "training_script_key",
      "hyperparameters",
      "inference_environment_variables",
      "inference_vulnerable",
      "inference_dependencies",
      "inference_vulnerabilities",
      "training_vulnerable",
      "training_dependencies",
      "training_vulnerabilities",
      "deprecated"
    )
  ),
  lock_objects = F
)

#' @title JumpStartVersionedModelId class
#' @description Data class for versioned model IDs.
#' @export
JumpStartVersionedModelId = R6Class("JumpStartVersionedModelId",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @description Instantiates JumpStartVersionedModelId object.
    #' @param model_id (str): JumpStart model ID.
    #' @param version (str): JumpStart model version.
    initialize = function(model_id,
                          version){
      self$model_id = model_id
      self$version = version
    }
  ),
  private = list(
    .slots = list(
      "model_id", "version"
    )
  ),
  lock_objects = F
)

#' @title JumpStartCachedS3ContentKey class
#' @description Data class for the s3 cached content keys.
#' @export
JumpStartCachedS3ContentKey = R6Class("JumpStartCachedS3ContentKey",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @description Instantiates JumpStartCachedS3ContentKey object.
    #' @param file_type (JumpStartS3FileType): JumpStart file type.
    #' @param s3_key (str): object key in s3.
    initialize = function(file_type,
                          s3_key){
      self$file_type = file_type
      self$s3_key = s3_key
    }
  ),
  private = list(
    .slots = list("file_type", "s3_key")
  ),
  lock_objecs = F
)

#' @title JumpStartCachedS3ContentValue class
#' @description Data class for the s3 cached content values.
#' @export
JumpStartCachedS3ContentValue = R6Class("JumpStartCachedS3ContentValue",
  inherit = JumpStartDataHolderType,
  public = list(

    #' @description Instantiates JumpStartCachedS3ContentValue object.
    #' @param formatted_content (Union[Dict[JumpStartVersionedModelId, JumpStartModelHeader],
    #'              JumpStartModelSpecs]): Formatted content for model specs and mappings from
    #'              versioned model IDs to specs.
    #' @param md5_hash (str): md5_hash for stored file content from s3.
    initialize = function(formatted_content,
                          md5_hash=NULL){
      self$formatted_content = formatted_content
      self$md5_hash = md5_hash
    }
  ),
  private = list(
    .slots = list("formatted_content", "md5_hash")
  ),
  lock_objects = F
)
