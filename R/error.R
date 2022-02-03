#' @import R6

#' @title SagemakerError
#' @description Create Sagemaker error format.
#' @keywords internal
#' @name SagemakerError
#' @export
SagemakerError = R6Class("SagemakerError",
  public = list(
   #' @description Initialize SagemakerError class
   #' @param ... (character): message to be outputted in error.
   initialize = function(...){
     private$.construct_msg_cls(...)
     stop(private$.construct_error_str())
   }
  ),
  private = list(
   .error_cls = NULL,
   .error_msg = NULL,
   .construct_msg_cls = function(...){
     msg_list = list(...)
     private$.error_cls = c(class(self)[-length(class(self))], "error", "condition")
     private$.error_msg = ifelse(
       length(msg_list) == 0,
       private$.error_cls[1],
       paste(msg_list, collapse = ""))
   },
   .construct_error_str = function(attributes = NULL){
     .Data = list(message = private$.error_msg)
     for(i in names(attributes)) .Data[[i]] = attributes[[i]]
     return(structure(.Data, class = private$.error_cls))
   }
  )
)

#' @rdname SagemakerError
#' @export
NotImplementedError = R6Class("NotImplementedError", inherit = SagemakerError)

#' @rdname SagemakerError
#' @export
ValueError = R6Class("ValueError", inherit = SagemakerError)

#' @rdname SagemakerError
#' @export
TypeError = R6Class("TypeError", inherit = SagemakerError)

# Raised when resource status is not expected and thus not allowed for further execution
#' @rdname SagemakerError
#' @export
UnexpectedStatusError = R6Class("UnexpectedStatusError",
  inherit = ValueError,
  public = list(

    #' @description Initialize UnexpectedStatusError class
    #' @param ... (character): message to be outputted in error.
    #' @param allowed_statuses (character): allowed status from sagemaker
    #' @param actual_status (character): returning status from aws sagemaker
    initialize = function(..., allowed_statuses, actual_status){
      private$.construct_msg_cls(...)
      stop(private$.construct_error_str(
        as.list(environment())))
    }
  )
)

#' @rdname SagemakerError
#' @export
RuntimeError = R6Class("RuntimeError", inherit = SagemakerError)

#' @rdname SagemakerError
#' @export
AttributeError = R6Class("AttributeError", inherit = SagemakerError)

#' @rdname SagemakerError
#' @export
DataNotFoundError = R6Class("DataNotFoundError", inherit = SagemakerError)

#' @rdname SagemakerError
#' @export
UnknownServiceError = R6Class("UnknownServiceError", inherit = DataNotFoundError)
