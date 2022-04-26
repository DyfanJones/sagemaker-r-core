# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/jumpstart/types.py

#' @import R6
#' @importfrom jsonlite toJSON
#' @importFrom stats setNames
#' @importFrom data.table address

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

# TODO: Up to here
JumpStartHyperparameter = R6Class("JumpStartHyperparameter",
  inherit = JumpStartDataHolderType,
  public = list(

  )
)






