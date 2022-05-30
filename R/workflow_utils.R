# Check if the variable is a pipeline variable
# Args:
#   var (object): The variable to be verified.
# Returns:
#   bool: True if it is, False otherwise.
is_pipeline_variable = function(var){

  # Currently Expression is on top of all kinds of pipeline variables
  # as well as PipelineExperimentConfigProperty and PropertyFile
  # TODO: We should deprecate the Expression and replace it with PipelineVariable
  return(inherits(var, "Expression"))
}
