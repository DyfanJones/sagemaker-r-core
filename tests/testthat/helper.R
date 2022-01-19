# Class to mock R6 classes
# used for testing only
Mock <- R6::R6Class("Mock",
  public = list(
    initialize = function(name, ...){
      if(!missing(name)) class(self) <- append(name, class(self))
      args = list(...)
      # dynamically assign public methods
      sapply(names(args), function(i) self[[i]] = args[[i]])
    },
    .call_args = function(name, return_value=NULL, side_effect = NULL){
      input_kwargs = list()
      i <- 0
      self[[name]] = function(..., ..return_value = FALSE, ..count=FALSE, ..return_value_all=FALSE){
        args = list(...)

        # Return latest parameter values
        if(..return_value) {
          if (i == 0)
            return(NULL)
          return(input_kwargs[[i]])
        }

        # Return all parameter values used
        # from multiple function calls
        if(..return_value_all)
          return(input_kwargs)

        # Return the count of how many times the function was called
        if(..count)
          return(i)

        # Add Parameter values to cache
        input_kwargs[[i+1]] <<- args
        i <<- i+1

        if(!is.null(side_effect)){
          return(side_effect(...))
        }
        return(return_value)
      }
    }
  ),
  lock_objects = F
)

# Basic mock function
mock_fun = function(return_value=NULL, side_effect = NULL){
  input_kwargs = list()
  i <- 0
  function(..., ..return_value = FALSE, ..count=FALSE, ..return_value_all=FALSE){
    args = list(...)

    # Return latest parameter values
    if(..return_value) {
      if (i == 0)
        return(NULL)
      return(input_kwargs[[i]])
    }

    # Return all parameter values used
    # from multiple function calls
    if(..return_value_all)
      return(input_kwargs)

    # Return the count of how many times the function was called
    if(..count)
      return(i)

    # Add Parameter values to cache
    input_kwargs[[i+1]] <<- args
    i <<- i+1

    if(!is.null(side_effect)){
      return(side_effect(...))
    }
    return(return_value)
  }
}

iter <- function(...) {
  return_value=list(...)
  value <- 1
  function(...) {
    if (value <= length(return_value)){
      item = return_value[[value]]
      value <<- value + 1
      return(item)
    } else {
      return(NULL)
    }
  }
}

# super basic function to unlock R6 environment bindings
unlockEnvironmentBinding = function(env){
  stopifnot(is.environment(env))
  env_names <- names(env)
  env_names_locked <- vapply(env_names, bindingIsLocked, env=env, FUN.VALUE=logical(1))
  lapply(env_names[env_names_locked], unlockBinding, env=env)
  invisible(TRUE)
}

mock_r6_private = function(r6_class, private_method, mock_fun){
  unlockEnvironmentBinding(r6_class$.__enclos_env__$private)
  assign(private_method, mock_fun, envir = r6_class$.__enclos_env__$private)
}

with_mock = function(..., mock_env = pkg_env(), eval_env = parent.frame()){
  mockthat::with_mock(..., eval_env = eval_env, mock_env = "sagemaker.common")
}

