#' @include r_utils.R
#' @include error.R

#' @import R6
#' @import paws
#' @importFrom utils getFromNamespace

#' @title PawsSession Class create connection to AWS utilizing paws.
#' @description A session stores configuration state and allows you to create paws service
#'              clients.
#' @export
PawsSession = R6Class("PawsSession",
  public = list(

    #' @field aws_access_key_id
    #' aws access key
    aws_access_key_id = NULL,

    #' @field aws_secret_access_key
    #' aws secret access key
    aws_secret_access_key = NULL,

    #' @field aws_session_token
    #' aws session token
    aws_session_token = NULL,

    #' @field region_name
    #' Default region when creating new connections
    region_name = NULL,

    #' @field profile_name
    #' The name of a profile to use.
    profile_name = NULL,

    #' @field endpoint
    #' The complete URL to use for the constructed client.
    endpoint = NULL,

    #' @field credentials
    #' Formatted aws credentials to pass to paws objects
    credentials = NULL,

    #' @description Initialize PawsSession class
    #' @param aws_access_key_id (str): AWS access key ID
    #' @param aws_secret_access_key (str): AWS secret access key
    #' @param aws_session_token (str): AWS temporary session token
    #' @param region_name (str): Default region when creating new connections
    #' @param profile_name (str): The name of a profile to use. If not given, then the default profile is used.
    #' @param endpoint (str): The complete URL to use for the constructed client.
    #' @param config (list): Optional paws configuration of credentials, endpoint, and/or region.
    initialize = function(aws_access_key_id = NULL,
                          aws_secret_access_key = NULL,
                          aws_session_token = NULL,
                          region_name = NULL,
                          profile_name = NULL,
                          endpoint = NULL,
                          config = list()){
      self$aws_access_key_id = aws_access_key_id %||% get_aws_env("AWS_ACCESS_KEY_ID")
      self$aws_secret_access_key = aws_secret_access_key %||% get_aws_env("AWS_SECRET_ACCESS_KEY")
      self$aws_session_token = aws_session_token %||% get_aws_env("AWS_SESSION_TOKEN")
      self$region_name = region_name %||% get_region(profile_name)
      self$profile_name = (
        if(!(is.null(self$aws_access_key_id) ||
             is.null(self$aws_secret_access_key) ||
             is.null(self$aws_session_token))) NULL
        else get_profile_name(profile_name)
      )
      self$endpoint = endpoint
      private$.cred_set()
      self$credentials = modifyList(self$credentials, config)
    },

    #' @description Create a low-level service client by name.
    #' @param service_name (str): The name of a service, e.g. 's3' or 'ec2'.
    #'              A list of available services can be found \url{https://paws-r.github.io/docs/}
    #' @param config (list): Optional paws configuration of credentials, endpoint, and/or region.
    client = function(service_name, config = NULL){
      stopifnot(is.character(service_name),
                is.null(config) || is.list(config))
      tryCatch({
        svc = getFromNamespace(service_name, "paws")
      },
      error = function(e){
        UnknownServiceError$new(sprintf(
          "Unknown service: '%s'. Valid service names are: %s",
          service_name, paste(ls(envir = asNamespace("paws")), collapse = ", ")
          )
        )
      })

      # Allow for credentials to be modified per client service if needed.
      cred = modifyList(self$credentials, as.list(config))
      return(svc(config = cred))
    },

    #' @description format class
    format = function(){
      format_class(self)
    }
  ),
  private = list(
    # set credentials
    .cred_set = function(){
      add_list <-function(x) if(length(x) == 0) NULL else x
      config <- list()
      credentials <- list()
      cred <- list()

      cred$access_key_id = self$aws_access_key_id
      cred$secret_access_key = self$aws_secret_access_key
      cred$session_token = self$aws_session_token

      credentials$creds <- add_list(cred)
      credentials$profile <- self$profile_name
      config$credentials <- add_list(credentials)
      config$endpoint <- self$endpoint
      config$region <- self$region_name

      self$credentials <- config
    }
  )
)
