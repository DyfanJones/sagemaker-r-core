# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/dev/src/sagemaker/serverless/serverless_inference_config.py

#' @import sagemaker.core

#' @title Configuration object passed in when deploying models to Amazon SageMaker Endpoints.
#' @description This object specifies configuration related to serverless endpoint. Use this configuration
#'              when trying to create serverless endpoint and make serverless inference
#' @export
ServerlessInferenceConfig = R6Class("ServerlessInferenceConfig",
  public = list(

    #' @field memory_size_in_mb
    #' The memory size of your serverless endpoint.
    memory_size_in_mb = NULL,

    #' @field max_concurrency
    #' The maximum number of concurrent invocations your serverless endpoint can process
    max_concurrency = NULL,

    #' @description Initialize a ServerlessInferenceConfig object for serverless inference configuration.
    #' @param memory_size_in_mb (int): Optional. The memory size of your serverless endpoint.
    #'              Valid values are in 1 GB increments: 1024 MB, 2048 MB, 3072 MB, 4096 MB,
    #'              5120 MB, or 6144 MB. If no value is provided, Amazon SageMaker will choose
    #'              the default value for you. (Default: 2048)
    #' @param max_concurrency (int): Optional. The maximum number of concurrent invocations
    #'              your serverless endpoint can process. If no value is provided, Amazon
    #'              SageMaker will choose the default value for you. (Default: 5)
    initialize = function(memory_size_in_mb=2048,
                          max_concurrency=5){
      self$memory_size_in_mb = memory_size_in_mb
      self$max_concurrency = max_concurrency
    },

    #' @description Generates a request dictionary using the parameters provided to the class.
    to_request_list = function(){
      request_list = list(
        "MemorySizeInMB"=self$memory_size_in_mb,
        "MaxConcurrency"=self$max_concurrency
      )
      return(request_list)
    },

    #' @description Format class
    format = function(){
      format_class(self)
    }
  )
)
