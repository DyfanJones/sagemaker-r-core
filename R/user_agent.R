# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/user_agent.py

#' @include r_utils.R

#' @import paws.common
#' @importFrom utils packageVersion

SDK_VERSION = function() utils::packageVersion(pkg_name())

determine_prefix = function() {
  prefix = sprintf("AWS-SageMaker-R-SDK/%s", SDK_VERSION())

  tryCatch({
    sagemaker_nbi_file = readLines("/etc/opt/ml/sagemaker-notebook-instance-version.txt", warn = FALSE)
    prefix = sprintf("%s AWS-SageMaker-Notebook-Instance/%s",
      prefix, trimws(sagemaker_nbi_file)
    )
  }, warning = function(w) {
    NULL
  }, error = function(e){
    NULL
  })
  return(prefix)
}

prepend_user_agent = function(){
  # TODO: Pseudo code for ticket: https://github.com/paws-r/paws/issues/499
  # user_agent_pattern = pkg_method("metadata_env", "paws.common")$user_agent_pattern
  # set_user_agent_pattern(paste(determine_prefix(), user_agent_pattern))
}
