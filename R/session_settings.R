# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/session_settings.py

#' @include r_utils.R

#' @import R6

#' @title Optional container class for settings to apply to a SageMaker session.
#' @export
SessionSettings = R6Class("SessionSettings",
  public = list(

    #' @description Initialize the ``SessionSettings`` of a SageMaker ``Session``.
    #' @param encrypt_repacked_artifacts (bool): Flag to indicate whether to encrypt the artifacts
    #'              at rest in S3 using the default AWS managed KMS key for S3 when a custom KMS key
    #'              is not provided (Default: \code{TRUE}).
    initialize = function(encrypt_repacked_artifacts = TRUE){
      private$.encrypt_repacked_artifacts = encrypt_repacked_artifacts
    },

    #' @description Format class
    format = function(){
      format_class(self)
    }
  ),
  active = list(

    #' @field encrypt_repacked_artifacts
    #' Return True if repacked artifacts at rest in S3 should be encrypted by default.
    encrypt_repacked_artifacts = function(){
      return(private$.encrypt_repacked_artifacts)
    }
  ),
  private = list(
    .encrypt_repacked_artifacts = NULL
  )
)
