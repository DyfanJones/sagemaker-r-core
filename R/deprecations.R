# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/deprecations.py


V2_URL = "https://sagemaker.readthedocs.io/en/stable/v2.html"

.warn <- function(msg){
  full_msg = sprintf("%s to align with python sagemaker>=2.\nSee: %s for details.", msg, V2_URL)
  warning(full_msg, call. = F)
  LOGGER$warn(full_msg)
}

#' @title Raise a warning for a no-op in sagemaker>=2
#' @param phrase (str): the prefix phrase of the warning message.
#' @export
removed_warning <- function(phrase){
  .warn(sprintf("%s is a no-op", phrase))
}

#' @title Raise a warning for a rename in sagemaker>=2
#' @param phrase (str): the prefix phrase of the warning message.
#' @export
renamed_warning <- function(phrase){
  .warn(sprintf("%s has been renamed", phrase))
}

#' @title Checks if the deprecated argument is in kwargs
#' @description Raises warning, if present.
#' @param old_name (str): name of deprecated argument
#' @param new_name (str): name of the new argument
#' @param value (str): value associated with new name, if supplied
#' @param kwargs (list): keyword arguments dict
#' @return value of the keyword argument, if present
#' @export
renamed_kwargs <- function(old_name,
                           new_name,
                           value,
                           kwargs){
  kwargs_name = deparse(substitute(kwargs))
  if(old_name %in% names(kwargs)){
    value = kwargs[[old_name]] %||% value
    kwargs[[new_name]] = value
    assign(kwargs_name, kwargs, envir = parent.frame())
    renamed_warning(old_name)}
  return(value)
}

#' @title Checks if the deprecated argument is populated.
#' @description Raises warning, if not None.
#' @param name (str): name of deprecated argument
#' @param arg (str): the argument to check
#' @export
remove_arg <- function(name,
                       arg = NULL){
  if(!is.null(arg)){
    removed_warning(name)
  }
}

#' @title Checks if the deprecated argument is in kwargs
#' @description Raises warning, if present.
#' @param name (str): name of deprecated argument
#' @param kwargs (str): keyword arguments dict
#' @export
removed_kwargs <- function(name,
                           kwargs){
  if (name %in% names(kwargs))
    removed_warning(name)
}

#' @title Wrap a function with a deprecation warning.
#' @param func (str): Function to wrap in a deprecation warning.
#' @param name (str): The name that has been deprecated.
#' @return The modified function
#' @export
deprecated_function <- function(func, name){
  deprecate <- function(...){
    renamed_warning(sprintf("The %s", name))
    return(do.call(func, list(...)))
  }
  return(deprecate)
}
